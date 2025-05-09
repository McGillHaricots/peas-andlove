
exec(open("dependencies.py").read())
exec(open("transformerBlocks.py").read())

import concurrent.futures
np.random.seed(126)  

def train_fold(loc, data, unique):

    test = data[data["loc"] == loc]
    train = data[data["loc"] != loc] 
    xTrain = train.drop(["Unnamed: 0","x","taxa","loc","pheno"],axis=1)
    yTrain = train["pheno"]
    xTest = test.drop(["Unnamed: 0","x","taxa","loc","pheno"],axis=1)
    yTest = test["pheno"]

    batch_size = 15
    
    #compute feature weights using training data
    feature_weights,pooling_weights= getWeights(xTrain,yTrain,xTest,yTest,batch_size)

    # Prepare tensors and dataloaders

    xTrain = pd.DataFrame(xTrain)
    yTrain = pd.DataFrame(yTrain)
    xTest = pd.DataFrame(xTest)
    yTest = pd.DataFrame(yTest)
    
    train_loader, test_loader = createTensors(xTrain,xTest, yTrain,yTest)

    # Transformer model definition
    src_vocab_size = int(unique)
    tgt_vocab_size = 1
    d_model = 300
    d_ff = 150
    num_heads = 5
    num_layers = 5
    max_seq_length = X.shape[1]
    dropout = 0.05
    criterion = lambda estimations, batch_y: focalLoss(beta=0.5, gamma=1, batch_y=batch_y, estimations=estimations)
    transformer = Transformer(src_vocab_size, tgt_vocab_size, d_model, num_heads, num_layers, d_ff, max_seq_length, dropout, feature_weights,pooling_weights)

    optimizer = torch.optim.SGD(transformer.parameters(), lr=0.0001)
    scheduler = torch.optim.lr_scheduler.StepLR(optimizer, step_size=25, gamma=0.5)
    transformer.apply(initialize_attention_weights)

    loss_values = []
    val_losses = []
    val_accuracies = []
    
    xTrain, xValid, yTrain, yValid = train_test_split(xTrain, yTrain, test_size=0.33, shuffle=True)
    y_valid_tensor = torch.tensor(yValid.values, dtype=torch.float32)
    x_valid_tensor = torch.tensor(xValid.values, dtype=torch.long)
    valid_dataset = TensorDataset(x_valid_tensor, y_valid_tensor)
    valid_loader = DataLoader(valid_dataset, batch_size=batch_size, shuffle=False, drop_last=True)


    # Training loop
    val_accuracy_prev = 0
    for epoch in range(40):
        transformer.train()
        train_loss = 0

        for batch_x, batch_y in valid_loader:
            batch_y = batch_y.squeeze(-1)
            optimizer.zero_grad()
            estimations = transformer(batch_x)
            loss = criterion(estimations, batch_y)
            loss.backward()
            optimizer.step()
            train_loss += loss.item()

        scheduler.step()
        avg_train_loss = train_loss / len(train_loader)
        loss_values.append(avg_train_loss)

        # Test after each epoch
        transformer.eval()
        val_loss = 0
        predictions = []
        true_vals = []

        with torch.no_grad():
            for batch_x, batch_y in valid_loader:
                batch_y = batch_y.squeeze(-1)
                preds = transformer(batch_x)
                loss = criterion(preds, batch_y)
                val_loss += loss.item()

                predictions.extend(preds.numpy())
                true_vals.extend(batch_y.numpy())

        avg_val_loss = val_loss / len(valid_loader)
        val_losses.append(avg_val_loss)
        val_accuracy, _ = pearsonr(predictions, true_vals)
        val_accuracies.append(val_accuracy)
        
        current_lr = optimizer.param_groups[0]['lr']
        print(f"Fold {fold}, Epoch {epoch} | Train Loss: {avg_train_loss:.4f} | Val Loss: {avg_val_loss:.4f} | Val Accuracy: {val_accuracy:.4f} | LR: {current_lr:.6f}")

    loss_csv_file = f"SY_optim_loss_fold{fold}_CDBN_Locations.csv"
    with open(loss_csv_file, mode='w', newline='') as file:
        writer = csv.writer(file)
        writer.writerow(["Epoch", "Train Loss", "Validation Loss", "Validation Accuracy"])
        for i in range(len(loss_values)):
            writer.writerow([i + 1, loss_values[i], val_losses[i], val_accuracies[i]])

    #test
    fold_result, values = evaluateModel(transformer,test_loader)   
    values= pd.DataFrame(values)
    values.to_csv(f"pred_true_SY_BLB_{loc}.csv")

    print(f"Finished fold {fold}")
    
    return fold_result


# Load and preprocess the data
data = pd.read_csv("fullDatasetSY_Updated.csv")
IDS = data["taxa"]

# Identify the number of unique tokens
X = data.drop(["Unnamed: 0", "x", "taxa", "loc", "pheno"], axis=1)
unique = X.stack().nunique()


# Set up cross-validation
locations = data["loc"].unique()
cv_results = []

# Use ProcessPoolExecutor to run folds in parallel
with concurrent.futures.ProcessPoolExecutor() as executor:
    futures = []
    fold = 1
    for loc in locations:
        futures.append(executor.submit(train_fold, loc, data, unique))
    # Collect the results
    for future in concurrent.futures.as_completed(futures):
        cv_results.append(future.result())

# Save accuracies for each fold
final_df = pd.DataFrame({
    "Location": location_names,
    "Accuracy": cv_results
})

final_df.to_csv("SY_CV_Accuracies_CDBN_Locations.csv", index=False)
