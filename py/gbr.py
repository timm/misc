import numpy as np
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from sklearn.ensemble import GradientBoostingRegressor
from sklearn.metrics import mean_squared_error

# Step 1: Generate synthetic data with 10 features
np.random.seed(42)
X = np.random.rand(1000, 10)  # 1000 samples, 10 features
y = (
    3 * X[:, 0] + 2 * X[:, 1] ** 2 + np.sin(2 * np.pi * X[:, 2])
    + np.random.normal(scale=0.2, size=1000)
)  # A nonlinear function with some noise

# Step 2: Split into training and testing sets
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

# Step 3: Train the Gradient Boosting Regressor
gb_reg = GradientBoostingRegressor(n_estimators=200, learning_rate=0.1, max_depth=4, random_state=42)
gb_reg.fit(X_train, y_train)

# Step 4: Make predictions
y_pred = gb_reg.predict(X_test)

# Step 5: Evaluate the model
mse = mean_squared_error(y_test, y_pred)
print(f"Mean Squared Error: {mse:.4f}")

# Step 6: Feature Importance
feature_importances = gb_reg.feature_importances_
plt.bar(range(10), feature_importances)
plt.xlabel("Feature Index")
plt.ylabel("Importance")
plt.title("Feature Importances in Gradient Boosting")
plt.show()

