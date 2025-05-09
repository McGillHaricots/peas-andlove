
# Multi-Head Attention
class MultiHeadAttention(nn.Module):
    def __init__(self, d_model, num_heads,feature_weights):
        super(MultiHeadAttention, self).__init__()
        assert d_model % num_heads == 0, "d_model must be divisible by num_heads"

        self.d_model = d_model
        self.num_heads = num_heads
        self.d_k = d_model // num_heads

        self.W_q = nn.Linear(d_model, d_model)
        self.W_k = nn.Linear(d_model, d_model)
        self.W_v = nn.Linear(d_model, d_model)
        self.W_o = nn.Linear(d_model, d_model)

        self.feature_weights = feature_weights

    def scaled_dot_product_attention(self, Q, K, V):
        attn_scores = torch.matmul(Q, K.transpose(-2, -1)) / math.sqrt(self.d_k)

        # Ensure feature_weights has the shape [batch_size, num_heads, seq_length, 1]
        feature_weights_expanded = self.feature_weights.unsqueeze(1).unsqueeze(-1)  # Shape: [batch_size, 1, seq_length, 1]
        feature_weights_expanded = feature_weights_expanded.expand(-1, self.num_heads, -1, -1)  # Shape: [batch_size, num_heads, seq_length, 1]
    
        # Apply the feature weights to the attention scores
        attn_scores = attn_scores * feature_weights_expanded

        output = torch.matmul(attn_scores, V)
        
        return output

    def split_heads(self, x):
        batch_size, seq_length, d_model = x.size()
        return x.view(batch_size, seq_length, self.num_heads, self.d_k).transpose(1, 2)

    def combine_heads(self, x):
        batch_size, _, seq_length, d_k = x.size()
        return x.transpose(1, 2).contiguous().view(batch_size, seq_length, self.d_model)

    def forward(self, Q, K, V):
        Q = self.split_heads(self.W_q(Q))
        K = self.split_heads(self.W_k(K))
        V = self.split_heads(self.W_v(V))

        attn_output = self.scaled_dot_product_attention(Q, K, V)
        attn_output = self.combine_heads(attn_output)
        output = self.W_o(attn_output)

        return output

# Position-Wise Feed Forward
class PositionWiseFeedForward(nn.Module):
    def __init__(self, d_model, d_ff):
        super(PositionWiseFeedForward, self).__init__()
        self.fc1 = nn.Linear(d_model, d_ff)
        self.fc2 = nn.Linear(d_ff, d_model)
        self.gelu = nn.GELU()

    def forward(self, x):
        return self.fc2(self.gelu(self.fc1(x)))

# Positional Encoding
class PositionalEncoding(nn.Module):
    def __init__(self, d_model, max_seq_length):
        super(PositionalEncoding, self).__init__()
        
        pe = torch.zeros(max_seq_length, d_model)
        position = torch.arange(0, max_seq_length, dtype=torch.float).unsqueeze(1)
        div_term = torch.exp(torch.arange(0, d_model, 2).float() * -(math.log(10000.0) / d_model))
        pe[:, 0::2] = torch.sin(position * div_term)
        pe[:, 1::2] = torch.cos(position * div_term)
        self.register_buffer('pe', pe.unsqueeze(0))

    def forward(self, x):
        return x + self.pe[:, :x.size(1)]

# Encoder Layer
class EncoderLayer(nn.Module):
    def __init__(self, d_model, num_heads, d_ff, dropout,feature_weights):
        super(EncoderLayer, self).__init__()
        self.self_attn = MultiHeadAttention(d_model, num_heads,feature_weights)
        self.feed_forward = PositionWiseFeedForward(d_model, d_ff)
        self.norm1 = nn.LayerNorm(d_model)
        self.norm2 = nn.LayerNorm(d_model)
        self.dropout = nn.Dropout(dropout)

    def forward(self, x):
        attn_output = self.self_attn(x, x, x)
        x = self.norm1(x + self.dropout(attn_output))
        ff_output = self.feed_forward(x)
        x = self.norm2(x + self.dropout(ff_output))
        return x


def initialize_attention_weights(module):
    if isinstance(module, MultiHeadAttention):
        nn.init.xavier_uniform_(module.W_q.weight)
        nn.init.xavier_uniform_(module.W_k.weight)
        nn.init.xavier_uniform_(module.W_v.weight)
        nn.init.xavier_uniform_(module.W_o.weight)
    if isinstance(module, nn.Linear):
        nn.init.xavier_uniform_(module.weight)
    elif isinstance(module, nn.Embedding):
        nn.init.xavier_uniform_(module.weight)
        
def focalLoss(beta,gamma,batch_y,estimations):
    
    beta = beta
    gamma = gamma
    abs_error = torch.abs(estimations - batch_y)
    
    loss = (torch.tanh(beta * abs_error) ** gamma * abs_error)

    return loss.mean()

# Transformer Model

class Transformer(nn.Module):
    def __init__(self, src_vocab_size,tgt_vocab_size, d_model, num_heads, num_layers, d_ff, max_seq_length, dropout,feature_weights,pooling_weights):
        super(Transformer, self).__init__()
        
        self.encoder_embedding = nn.Embedding(src_vocab_size, d_model)
        self.positional_encoding = PositionalEncoding(d_model, max_seq_length)
        self.encoder_layers = nn.ModuleList([EncoderLayer(d_model, num_heads, d_ff, dropout,feature_weights) for _ in range(num_layers)])
        self.fc = nn.Linear(d_model, tgt_vocab_size)
        self.dropout = nn.Dropout(dropout)
        self.feature_weights = feature_weights[:, :max_seq_length]  
        self.pooling_weights = pooling_weights[:, :max_seq_length]

    def forward(self, src):

        src_embedded = self.dropout(self.positional_encoding(self.encoder_embedding(src)))
        
        for enc_layer in self.encoder_layers:
            enc_output = enc_layer(src_embedded)
        
        weighted_output = enc_output * self.pooling_weights.unsqueeze(-1)
        pooled_output = weighted_output.mean(dim=1)
        output = self.fc(pooled_output)
        return output.squeeze(-1)

def getWeights(xTrain,yTrain,xTest,yTest,batch_size):

    batch_size = batch_size
    alpha_values = {}
    params = [0.01, 0.1, 1, 10, 100, 1000]
    for value in params:
        LR = linear_model.Ridge(value)
        LR.fit(xTrain, yTrain)
        y_pred = LR.predict(xTest)
        yTest = np.array(yTest)
        alpha_values[value] = np.corrcoef(y_pred, yTest)[0, 1]
        alpha_val = max(alpha_values, key=alpha_values.get)

    # Ridge regression to get the coefficients (feature weights)
    RR = linear_model.Ridge(alpha_val)
    RR.fit(xTrain, yTrain)
    coeffs = RR.coef_
    coeffs = pd.DataFrame(coeffs).squeeze(1)
    min_val = coeffs.min()
    max_val = coeffs.max()
    scaled_coeffs = (coeffs - min_val) / (max_val - min_val)
    

    feature_weights = torch.tensor(scaled_coeffs, dtype=torch.float32)
    pooling_weights = torch.tensor(coeffs,dtype=torch.float32)
    feature_weights = feature_weights.view(1, -1).expand(batch_size, -1)
    pooling_weights = pooling_weights.view(1,-1).expand(batch_size,-1)

    return feature_weights, pooling_weights

def preprocess(data):
    
    data = data.sample(frac=1)
    X = data.drop(["Unnamed: 0", "0", "1", "2", "3"], axis=1)
    threshold = 0.01
    X = X.drop(X.std()[X.std() < threshold].index.values, axis=1)
    y = data["3"]
    # Identify the number of unique tokens
    unique = X.stack().nunique()
    return X,y,unique

def evaluateModel(model, loader):
    model.eval()
    predictions = []
    true_vals = []

    with torch.no_grad():
        for batch_x, batch_y in loader:
            batch_y = batch_y.squeeze(-1)
            preds = model(batch_x)
            loss = criterion(preds, batch_y)
            predictions.extend(preds.numpy())
            true_vals.extend(batch_y.numpy())

    accuracy, _ = pearsonr(predictions, true_vals)
    values = np.column_stack((predictions, true_vals))
    values = pd.DataFrame(values)
    values.columns = ["pred", "true"]

    return accuracy, values


def createTensors(xTrain, X_test, yTrain,y_test):
    xTrain_tensor = torch.tensor(xTrain.values, dtype=torch.long)
    yTrain_tensor = torch.tensor(yTrain.values, dtype=torch.float32)
    X_test_tensor = torch.tensor(X_test.values, dtype=torch.long)
    y_test_tensor = torch.tensor(y_test.values, dtype=torch.float32)

    train_dataset = TensorDataset(xTrain_tensor, yTrain_tensor)
    test_dataset = TensorDataset(X_test_tensor, y_test_tensor)

    train_loader = DataLoader(train_dataset, batch_size=batch_size, shuffle=False, drop_last=True)
    test_loader = DataLoader(test_dataset, batch_size=batch_size, shuffle=False, drop_last=True)

    return train_loader, test_loader
