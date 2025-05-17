import inspect
import sys

def inspect_file(filepath=None):
    """
    Inspect classes in the current file or a specified file.
    
    Args:
        filepath: Optional path to the file to inspect. If None, uses the current file.
    """
    if filepath is None:
        # Get classes from the current module
        current_module = sys.modules['__main__']
        classes = {name: obj for name, obj in inspect.getmembers(current_module, inspect.isclass)
                  if obj.__module__ == current_module.__name__}
    else:
        # Load the module from filepath
        import importlib.util
        spec = importlib.util.spec_from_file_location("module.name", filepath)
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        classes = {name: obj for name, obj in inspect.getmembers(module, inspect.isclass)
                  if obj.__module__ == module.__name__}
    
    # Print info for each class
    for class_name, class_obj in classes.items():
        print(f"\n{'='*50}")
        print(f"CLASS: {class_name}")
        if class_obj.__doc__:
            print(f"DOCSTRING: {class_obj.__doc__.strip()}")
        
        # Get attributes that aren't methods or special attributes
        attrs = {name: attr for name, attr in vars(class_obj).items() 
                if not name.startswith('__') and not inspect.isfunction(attr)}
        
        if attrs:
            print("\nATTRIBUTES:")
            for attr_name, attr_value in attrs.items():
                print(f"  {attr_name}: {attr_value}")
        
        # Get methods
        methods = inspect.getmembers(class_obj, 
                                    lambda x: inspect.isfunction(x) and x.__module__ == class_obj.__module__)
        
        if methods:
            print("\nMETHODS:")
            for method_name, method_obj in methods:
                if method_name.startswith('__'):
                    continue
                print(f"  {method_name}{inspect.signature(method_obj)}")
                if method_obj.__doc__:
                    doc = method_obj.__doc__.strip().split('\n')[0]  # First line only
                    print(f"    DOCSTRING: {doc}")

if __name__ == "__main__":
    # Usage examples:
    # 1. To inspect current file: inspect_file()
    # 2. To inspect another file: inspect_file("/path/to/file.py")
    inspect_file(sys.argv[1])
