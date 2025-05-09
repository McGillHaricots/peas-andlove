exec(open("dependencies.py").read())
exec(open("transformerBlocks.py").read())

'''
load data
'''

np.random.seed(126)

data = pd.read_csv("BLB_fullset.csv")
data = data.sample(frac =1)

'''
fold cross validation
'''
cv_results = []
true_pred = []

'''
train test split
'''

yield18 = data[["pheno.yd18","pheno.taxa"]]
yield19 = data[["pheno.yd19","pheno.taxa"]]
geno = data.filter(regex='geno')
entries = data[['pheno.taxa']]
entries.columns = ['pheno.taxa']
geno = pd.concat([entries,geno],axis=1)

train = pd.merge(yield19, geno, on="pheno.taxa")
test = pd.merge(yield18, geno, on="pheno.taxa")
train = train.dropna()
test = test.dropna()

xTrain = train.drop(["pheno.yd19","pheno.taxa"],axis=1)
yTrain = train["pheno.yd19"]
xTest = test.drop(["pheno.yd18","pheno.taxa"],axis=1)
yTest = test['pheno.yd18']

unique = xTrain.stack().nunique()
batch_size = 15

#compute feature weights using training data
feature_weights,pooling_weights= getWeights(xTrain,yTrain,xTest,yTest,batch_size)

# Prepare tensors and dataloaders

xTrain = pd.DataFrame(xTrain)
yTrain = pd.DataFrame(yTrain)
xTest = pd.DataFrame(xTest)
yTest = pd.DataFrame(yTest)

train_loader, test_loader = createTensors(xTrain,xTest, yTrain,yTest)



'''
define transformer parameters
'''
src_vocab_size = int(unique)
tgt_vocab_size = 1
d_model = 200
num_heads = 5
num_layers = 5
d_ff = 100
max_seq_length = xTrain.shape[1]
dropout = 0.05
criterion = lambda estimations, batch_y: focalLoss(beta=0.5, gamma=1, batch_y=batch_y, estimations=estimations)

transformer = Transformer(src_vocab_size, tgt_vocab_size, d_model, num_heads, num_layers, d_ff, max_seq_length, dropout,feature_weights,pooling_weights)

optimizer = torch.optim.Adam(transformer.parameters(), lr=0.001)
scheduler = torch.optim.lr_scheduler.StepLR(optimizer, step_size=45, gamma=0.1)
transformer.apply(initialize_attention_weights)

loss_values = []
val_losses = []
val_accuracies = []

xTrain, xValid, yTrain, yValid = train_test_split(xTrain, yTrain, test_size=0.33, shuffle=True)
y_valid_tensor = torch.tensor(yValid.values, dtype=torch.float32)
x_valid_tensor = torch.tensor(xValid.values, dtype=torch.long)
valid_dataset = TensorDataset(x_valid_tensor, y_valid_tensor)
valid_loader = DataLoader(valid_dataset, batch_size=batch_size, shuffle=False, drop_last=True)

'''
training loop
'''
val_accuracy_prev=0
for epoch in range(50):
    
    transformer.train()
    train_loss = 0
    
    for batch_x, batch_y in train_loader:
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
    print(f"Epoch {epoch} | Train Loss: {avg_train_loss:.4f} | Val Loss: {avg_val_loss:.4f} | Val Accuracy: {val_accuracy:.4f} | LR: {current_lr:.6f}")

'''
save results for best epoch
'''
cv_results.append(max(val_accuracies))

'''
save trained model for each fold
'''
torch.save(transformer, f"transformerSY_BLB_yd2018.pth")

'''
save training loss, validation loss, and validation accuracy
'''
loss_csv_file = f"SY_optim_loss_BLB_yd2018.csv"
with open(loss_csv_file, mode='w', newline='') as file:
    writer = csv.writer(file)
    writer.writerow(["Epoch", "Train Loss", "Validation Loss", "Validation Accuracy"])
    for i in range(len(loss_values)):
        writer.writerow([i + 1, loss_values[i], val_losses[i], val_accuracies[i]])
        
accuracy, values = evaluateModel(transformer,test_loader)
values = pd.DataFrame(values)
values.to_csv(f"pred_true_BLB_yd2018.csv")
pd.DataFrame([accuracy], columns=["Pearson"]).to_csv("SY_CV_Accuracies_BLB_yd2018.csv", index=False)
                             
