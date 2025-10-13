/*
 * test_cognitive_mesh_api.cc
 *
 * Integration tests for Cognitive Mesh API
 */

#include "../../api/CognitiveMeshAPI.h"
#include "../../api/EmbodimentTensorSignature.h"

#include <iostream>
#include <cassert>
#include <thread>
#include <chrono>

using namespace opencog::cognitive_mesh;
using namespace opencog::embodiment;

class CognitiveMeshAPITester {
private:
    std::unique_ptr<CognitiveMeshAPI> api_;
    
public:
    CognitiveMeshAPITester() {
        api_ = std::make_unique<CognitiveMeshAPI>(8080, 8081);
    }
    
    void test_api_lifecycle() {
        std::cout << "Testing API lifecycle..." << std::endl;
        
        // Test initial state
        assert(!api_->is_running());
        
        // Test start
        bool started = api_->start();
        assert(started);
        assert(api_->is_running());
        
        // Test stop
        api_->stop();
        assert(!api_->is_running());
        
        std::cout << "✓ API lifecycle test passed" << std::endl;
    }
    
    void test_agent_state_management() {
        std::cout << "Testing agent state management..." << std::endl;
        
        // Start API
        api_->start();
        
        // Create test agent state
        CognitiveStateSnapshot state;
        state.agent_id = "test_agent_001";
        state.cognitive_load = 0.75;
        state.active_atoms = {"atom1", "atom2", "atom3"};
        state.tensor_signature = EmbodimentTensorSignature("test_agent_001");
        state.timestamp = std::chrono::steady_clock::now();
        
        // Test state update
        auto response = api_->update_agent_state("test_agent_001", state);
        assert(response.success);
        assert(response.status_code == 200);
        
        // Test state retrieval
        response = api_->get_agent_state("test_agent_001");
        assert(response.success);
        assert(response.status_code == 200);
        assert(!response.body.empty());
        
        // Test non-existent agent
        response = api_->get_agent_state("non_existent_agent");
        assert(!response.success);
        assert(response.status_code == 404);
        
        api_->stop();
        std::cout << "✓ Agent state management test passed" << std::endl;
    }
    
    void test_embodiment_tensor_integration() {
        std::cout << "Testing embodiment tensor integration..." << std::endl;
        
        api_->start();
        
        // Create embodiment tensor
        EmbodimentTensorSignature tensor("test_agent_002");
        tensor.update_sensory_input(EmbodimentTensorSignature::VISUAL, 0.8);
        tensor.update_motor_command(EmbodimentTensorSignature::POSITION, 1.2);
        tensor.update_spatial_position(1.0, 2.0, 3.0, 0.5);
        
        // Create agent state with tensor
        CognitiveStateSnapshot state;
        state.agent_id = "test_agent_002";
        state.tensor_signature = tensor;
        state.cognitive_load = tensor.calculate_cognitive_load();
        
        // Update agent state
        auto response = api_->update_agent_state("test_agent_002", state);
        assert(response.success);
        
        // Test tensor retrieval
        response = api_->get_embodiment_tensor("test_agent_002");
        assert(response.success);
        assert(response.status_code == 200);
        assert(!response.body.empty());
        
        // Validate tensor data in response
        assert(response.body.find("tensor_magnitude") != std::string::npos);
        assert(response.body.find("cognitive_load") != std::string::npos);
        
        api_->stop();
        std::cout << "✓ Embodiment tensor integration test passed" << std::endl;
    }
    
    void run_all_tests() {
        std::cout << "=== Starting Cognitive Mesh API Integration Tests ===" << std::endl;
        
        test_api_lifecycle();
        test_agent_state_management();
        test_embodiment_tensor_integration();
        
        std::cout << "=== All Cognitive Mesh API tests passed! ===" << std::endl;
    }
};

int main() {
    try {
        CognitiveMeshAPITester tester;
        tester.run_all_tests();
        return 0;
    } catch (const std::exception& e) {
        std::cerr << "Test failed with exception: " << e.what() << std::endl;
        return 1;
    } catch (...) {
        std::cerr << "Test failed with unknown exception" << std::endl;
        return 1;
    }
}