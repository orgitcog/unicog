# JSON Serialization Fix for NumPy Types

## Issue
The Cognitive Membrane Sync workflow was encountering a `TypeError: Object of type int64 is not JSON serializable` error when attempting to serialize NumPy types to JSON.

## Root Cause
NumPy types (especially `np.int64`, `np.float64`, etc.) are not natively serializable by Python's standard `json` module. When NumPy arrays or numeric operations return these types, they must be explicitly converted to native Python types before JSON serialization.

## Solution

### 1. Centralized JSON Encoder Utility
Created `json_encoder_utils.py` providing multiple approaches to handle NumPy type serialization:

#### A. EnhancedJSONEncoder Class
```python
from json_encoder_utils import safe_json_dump

# Use instead of json.dump()
with open('output.json', 'w') as f:
    safe_json_dump(data, f, indent=2)
```

#### B. Callback Function
```python
from json_encoder_utils import enhanced_json_encoder
import json

# Use with standard json.dump()
json.dump(data, f, default=enhanced_json_encoder, indent=2)
```

#### C. Deep Conversion
```python
from json_encoder_utils import deep_convert

# Convert entire structure before serialization
data = deep_convert(data)
json.dump(data, f, indent=2)
```

### 2. Supported NumPy Types
The encoder handles:
- **Integers**: `np.int8`, `np.int16`, `np.int32`, `np.int64`, `np.uint8`, `np.uint16`, `np.uint32`, `np.uint64`
- **Floats**: `np.float16`, `np.float32`, `np.float64`
- **Booleans**: `np.bool_`
- **Arrays**: `np.ndarray` (converted to lists)
- **Generic**: All `np.generic` types

### 3. Implementation in Workflow
For GitHub Actions inline scripts, the encoder class is defined directly:

```python
class EnhancedJSONEncoder(json.JSONEncoder):
    def default(self, obj):
        if isinstance(obj, (np.integer, ...)):
            return int(obj)
        if isinstance(obj, (np.floating, ...)):
            return float(obj)
        if isinstance(obj, np.ndarray):
            return obj.tolist()
        return super().default(obj)

# Usage
json.dump(config, f, indent=2, cls=EnhancedJSONEncoder)
```

### 4. Explicit Conversions
For operations that definitely return NumPy types:

```python
# Before: Can cause serialization error
'degrees_of_freedom': np.prod(tensor_shape)

# After: Explicitly convert to int
'degrees_of_freedom': int(np.prod(tensor_shape))
```

## Files Modified
1. **cognitive-membrane-sync/json_encoder_utils.py** (NEW)
   - Centralized JSON encoding utilities
   - 5+ different approaches to handle NumPy types
   - Comprehensive documentation

2. **cognitive_membrane_scanner.py**
   - Import `safe_json_dump`
   - Replace `json.dump()` with `safe_json_dump()`

3. **cognitive-membrane-sync/membrane_bridge.py**
   - Import `safe_json_dump`
   - Replace `json.dump()` with `safe_json_dump()`

4. **cognitive-membrane-sync/demonstrate_framework.py**
   - Import `safe_json_dump`
   - Replace `json.dump()` with `safe_json_dump()`

5. **cognitive-membrane-sync/cognitive_membrane_cli.py**
   - Import `safe_json_dump`
   - Replace `json.dump()` with `safe_json_dump()`

6. **.github/workflows/cognitive-membrane-sync.yml**
   - Added `EnhancedJSONEncoder` class to inline scripts (2 locations)
   - Updated all `json.dump()` calls to use `cls=EnhancedJSONEncoder`
   - Added explicit `int()` conversions for `np.prod()` results

## Testing
All tests pass successfully:
- ✅ `demonstrate_framework.py` runs without errors
- ✅ Generated JSON files are valid
- ✅ No NumPy types remain in serialized data
- ✅ CLI commands work correctly
- ✅ Workflow inline scripts tested

## Best Practices

### For Python Scripts
Always use the centralized utility:
```python
from json_encoder_utils import safe_json_dump

# Instead of:
# with open('file.json', 'w') as f:
#     json.dump(data, f, indent=2)

# Use:
with open('file.json', 'w') as f:
    safe_json_dump(data, f, indent=2)
```

### For GitHub Actions
Include the encoder class in your inline script:
```yaml
- name: Generate Data
  run: |
    cat > script.py << 'EOF'
    import json
    import numpy as np
    
    class EnhancedJSONEncoder(json.JSONEncoder):
        def default(self, obj):
            if isinstance(obj, np.integer):
                return int(obj)
            if isinstance(obj, np.floating):
                return float(obj)
            if isinstance(obj, np.ndarray):
                return obj.tolist()
            return super().default(obj)
    
    # Use cls parameter
    json.dump(data, f, cls=EnhancedJSONEncoder)
    EOF
```

### For Explicit Conversions
When you know a value is a NumPy type:
```python
# Wrap in int() or float()
result = {
    'count': int(np.sum(array)),
    'mean': float(np.mean(array)),
    'shape': list(array.shape)  # tuple to list
}
```

## Future Considerations

1. **Performance**: The encoder adds minimal overhead as conversion only happens during serialization

2. **Pandas DataFrames**: If using pandas, convert dtypes before serialization:
   ```python
   df = df.astype(object)  # Convert to native Python types
   data = df.to_dict()
   safe_json_dump(data, f)
   ```

3. **Deep Nesting**: For deeply nested structures with many NumPy types, consider using `deep_convert()`:
   ```python
   from json_encoder_utils import deep_convert
   
   data = deep_convert(complex_nested_structure)
   json.dump(data, f, indent=2)
   ```

4. **Custom Types**: Extend the encoder for additional types as needed:
   ```python
   class CustomEncoder(EnhancedJSONEncoder):
       def default(self, obj):
           if isinstance(obj, MyCustomType):
               return obj.to_dict()
           return super().default(obj)
   ```

## Issue Resolution
This fix resolves the error reported in Job 56154637130 (🔮 Cognitive Membrane Sync):
```
TypeError: Object of type int64 is not JSON serializable
```

The solution is comprehensive, maintainable, and follows Python best practices for JSON serialization with NumPy types.

## References
- Python json module: https://docs.python.org/3/library/json.html
- NumPy data types: https://numpy.org/doc/stable/reference/arrays.scalars.html
- JSON encoder customization: https://docs.python.org/3/library/json.html#json.JSONEncoder
