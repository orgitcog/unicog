#include <opencog/util/sigslot.h>
#include <iostream>
#include <vector>
#include <string>
#include <cassert>

// Test class with various member functions
class TestClass {
public:
    int callback_count = 0;
    int last_x = 0;
    std::vector<int> last_vec;
    std::string last_str;

    void simple_callback(int x) {
        callback_count++;
        last_x = x;
        std::cout << "TestClass::simple_callback called with: " << x << std::endl;
    }

    void complex_callback(int x, std::vector<int> vec) {
        callback_count++;
        last_x = x;
        last_vec = vec;
        std::cout << "TestClass::complex_callback called with: " << x << " and vector of size " << vec.size() << std::endl;
    }

    void const_callback(int x) const {
        std::cout << "TestClass::const_callback called with: " << x << std::endl;
    }

    void string_callback(std::string str, int x) {
        callback_count++;
        last_str = str;
        last_x = x;
        std::cout << "TestClass::string_callback called with: '" << str << "' and " << x << std::endl;
    }
};

// Free function for comparison
void free_function(int x) {
    std::cout << "free_function called with: " << x << std::endl;
}

void free_function2(int x, std::vector<int> vec) {
    std::cout << "free_function2 called with: " << x << " and vector of size " << vec.size() << std::endl;
}

int main() {
    std::cout << "=== SigSlot Test Suite ===" << std::endl;

    // Test 1: Basic member function connection
    {
        std::cout << "\nTest 1: Basic member function connection" << std::endl;
        TestClass obj;
        SigSlot<int> sig;
        
        int id = sig.connect_member(&TestClass::simple_callback, &obj);
        assert(id > 0);
        
        sig.emit(42);
        assert(obj.callback_count == 1);
        assert(obj.last_x == 42);
    }

    // Test 2: Multiple arguments
    {
        std::cout << "\nTest 2: Multiple arguments" << std::endl;
        TestClass obj;
        SigSlot<int, std::vector<int>> sig;
        
        sig.connect_member(&TestClass::complex_callback, &obj);
        sig.emit(100, {1, 2, 3, 4, 5});
        
        assert(obj.callback_count == 1);
        assert(obj.last_x == 100);
        assert(obj.last_vec.size() == 5);
        assert(obj.last_vec[0] == 1);
    }

    // Test 3: Const member function
    {
        std::cout << "\nTest 3: Const member function" << std::endl;
        const TestClass obj;
        SigSlot<int> sig;
        
        sig.connect_member(&TestClass::const_callback, &obj);
        sig.emit(200);
        // Visual verification only for const method
    }

    // Test 4: Multiple connections
    {
        std::cout << "\nTest 4: Multiple connections" << std::endl;
        TestClass obj1, obj2;
        SigSlot<int> sig;
        
        int id1 = sig.connect_member(&TestClass::simple_callback, &obj1);
        int id2 = sig.connect_member(&TestClass::simple_callback, &obj2);
        
        sig.emit(300);
        assert(obj1.callback_count == 1);
        assert(obj2.callback_count == 1);
        assert(obj1.last_x == 300);
        assert(obj2.last_x == 300);
        
        // Test disconnect
        sig.disconnect(id1);
        sig.emit(400);
        assert(obj1.callback_count == 1); // Should not increase
        assert(obj2.callback_count == 2); // Should increase
    }

    // Test 5: Mix of free functions and member functions
    {
        std::cout << "\nTest 5: Mix of free functions and member functions" << std::endl;
        TestClass obj;
        SigSlot<int> sig;
        
        sig.connect(free_function);
        sig.connect_member(&TestClass::simple_callback, &obj);
        
        sig.emit(500);
        assert(obj.callback_count == 1);
    }

    // Test 6: Using std::bind with member functions (original example from comments)
    {
        std::cout << "\nTest 6: Using std::bind with member functions" << std::endl;
        TestClass obj;
        SigSlot<int, std::vector<int>> sig;
        
        auto bound_fn = std::bind(&TestClass::complex_callback, &obj, 
                                  std::placeholders::_1, std::placeholders::_2);
        sig.connect(bound_fn);
        
        sig.emit(600, {10, 20, 30});
        assert(obj.callback_count == 1);
        assert(obj.last_x == 600);
        assert(obj.last_vec.size() == 3);
    }

    // Test 7: String and mixed types
    {
        std::cout << "\nTest 7: String and mixed types" << std::endl;
        TestClass obj;
        SigSlot<std::string, int> sig;
        
        sig.connect_member(&TestClass::string_callback, &obj);
        sig.emit("Hello World", 700);
        
        assert(obj.callback_count == 1);
        assert(obj.last_str == "Hello World");
        assert(obj.last_x == 700);
    }

    // Test 8: disconnect_all
    {
        std::cout << "\nTest 8: disconnect_all" << std::endl;
        TestClass obj1, obj2;
        SigSlot<int> sig;
        
        sig.connect_member(&TestClass::simple_callback, &obj1);
        sig.connect_member(&TestClass::simple_callback, &obj2);
        assert(sig.size() == 2);
        
        sig.disconnect_all();
        assert(sig.size() == 0);
        
        sig.emit(800); // Should not call any callbacks
        assert(obj1.callback_count == 0);
        assert(obj2.callback_count == 0);
    }

    std::cout << "\n=== All tests passed! ===" << std::endl;
    return 0;
}