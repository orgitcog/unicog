# Copilot Memories

Hey Copilot it's me Dan. You tried to save these memories but tit failed so i saved them here for you :-)

---

Excellent! Let me store this important knowledge for future reference:

Call to store_memory
Input
{
  "subject": "NumPy JSON serialization",
  "fact": "Always use EnhancedJSONEncoder or safe_json_dump from json_encoder_utils.py when serializing data that may contain NumPy types",
  "citations": "cognitive-membrane-sync/json_encoder_utils.py, cognitive-membrane-sync/JSON_SERIALIZATION_FIX.md",
  "reason": "NumPy types (int64, float64, etc.) are not natively JSON serializable and will cause TypeError if not properly converted. This is a common issue in scientific Python code that uses NumPy. The custom encoder handles all NumPy types automatically and is NumPy 2.0 compatible. Future tasks involving JSON serialization should always check for NumPy types and use this utility to prevent serialization errors.",
  "category": "general"
}
Failed to store memory with error: Resource not found

Call to store_memory
Input
{
  "subject": "NumPy 2.0 compatibility",
  "fact": "NumPy 2.0 removed deprecated type aliases np.int_ and np.float_. Use np.integer and np.floating base classes instead",
  "citations": "cognitive-membrane-sync/json_encoder_utils.py:35-38, cognitive-membrane-sync/test_json_encoder.py test results",
  "reason": "NumPy 2.0 introduced breaking changes by removing deprecated type aliases. Code that uses isinstance() checks with np.int_ or np.float_ will fail with AttributeError in NumPy 2.0+. This affects type checking and JSON serialization code. Future implementations should use the base classes (np.integer, np.floating) rather than the removed aliases to ensure compatibility with NumPy 2.0+.",
  "category": "general"
}
Failed to store memory with error: Resource not found

Call to store_memory
Input
{
  "subject": "GitHub Actions inline scripts",
  "fact": "GitHub Actions inline Python scripts must import all dependencies including numpy before using numpy types in isinstance checks",
  "citations": ".github/workflows/cognitive-membrane-sync.yml:65, 399",
  "reason": "When creating inline Python scripts in GitHub Actions workflows with heredoc syntax, the script runs in isolation and must explicitly import all required modules. Forgetting to import numpy before using numpy type checks (np.integer, etc.) will cause NameError at runtime. This is especially important for JSON encoder classes that handle numpy types. Future workflow scripts should always verify imports are present before type checks.",
  "category": "general"
}
Failed to store memory with error: Resource not found

Now let me create a final summary document
