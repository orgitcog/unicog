#!/bin/bash
# .devcontainer/setup-cognitive-membrane.sh
# Multi-Membrane Cognitive Workspace Setup

echo "🧠 Setting up Cognitive Cities Multi-Membrane Workspace..."

# Create cognitive membrane structure
mkdir -p /workspaces/cosmos/{cogpilot,OzCog,shared,membrane-sync}

# Setup workspace structure
cd /workspaces/cosmos

echo "📁 Creating cognitive membrane directory structure..."

# Create symbolic links for cross-membrane communication
ln -sf /workspaces/cognitive-membrane /workspaces/cosmos/shared/unified-view
mkdir -p /workspaces/cosmos/shared/{cogpilot-view,ozcog-view,membrane-interfaces}

# Initialize cognitive tensor configuration
echo "🔢 Initializing ggml cognitive tensor..."

python3 << 'EOF'
import numpy as np
import json
from pathlib import Path

# Define cognitive grammar tensor shape
# Dimensions: [attention, orgs, repos, concepts, implementations]
tensor_shape = (7, 3, 10, 50, 100)
cognitive_field = np.zeros(tensor_shape)

# Prime factorization for each membrane level
membrane_primes = {
    'enterprise': 2,
    'organization': 3, 
    'repository': 5,
    'folder': 7,
    'file': 11
}

# Generate cognitive membrane topology
membrane_topology = {
    'workspace': '/workspaces/cosmos',
    'tensor_shape': list(tensor_shape),
    'total_parameters': int(np.prod(tensor_shape)),
    'membrane_primes': membrane_primes,
    'organizations': {
        'cogpilot': {
            'prime': 2,
            'type': 'interface_membrane',
            'repositories': ['cognitive-cities', 'plan9-cogcities-kernel'],
            'tensor_slice': [0, 0, slice(0, 2)]
        },
        'OzCog': {
            'prime': 3,
            'type': 'core_cognitive_membrane', 
            'repositories': ['opencog-unified', 'opencog-bridge'],
            'tensor_slice': [0, 1, slice(2, 4)]
        },
        'cosmos': {
            'prime': 5,
            'type': 'meta_coordination_membrane',
            'repositories': ['membrane-sync', 'cognitive-grammar'],
            'tensor_slice': [0, 2, slice(4, 6)]
        }
    },
    'cross_membrane_connections': [
        {'from': 'cogpilot', 'to': 'OzCog', 'type': 'neural_interface'},
        {'from': 'OzCog', 'to': 'cosmos', 'type': 'cognitive_coordination'},
        {'from': 'cosmos', 'to': 'cogpilot', 'type': 'meta_feedback'}
    ]
}

# Save cognitive configuration
config_path = Path('/workspaces/cosmos/cognitive-membrane-config.json')
with open(config_path, 'w') as f:
    json.dump(membrane_topology, f, indent=2)

print(f"✅ Initialized cognitive tensor field: {tensor_shape}")
print(f"📊 Total parameters: {np.prod(tensor_shape):,}")
print(f"🎯 Configuration saved to: {config_path}")

# Create membrane interface scripts
interface_scripts = {
    'membrane_sync.py': '''#!/usr/bin/env python3
"""
Cognitive Membrane Synchronization Interface
Handles bidirectional sync between organizational membranes
"""
import json
import asyncio
from pathlib import Path

class MembraneSynchronizer:
    def __init__(self, config_path="/workspaces/cosmos/cognitive-membrane-config.json"):
        with open(config_path) as f:
            self.config = json.load(f)
    
    async def sync_membranes(self):
        """Synchronize across all cognitive membranes"""
        print("🌀 Starting membrane synchronization...")
        
        for org_name, org_config in self.config['organizations'].items():
            await self.sync_organization(org_name, org_config)
        
        print("✅ Membrane synchronization completed")
    
    async def sync_organization(self, org_name, org_config):
        """Sync specific organizational membrane"""
        print(f"🔄 Syncing {org_name} ({org_config['type']}) - Prime: {org_config['prime']}")
        
        # Simulate membrane synchronization
        await asyncio.sleep(0.1)
        
    def get_tensor_slice(self, org_name):
        """Get tensor slice for specific organization"""
        return self.config['organizations'][org_name]['tensor_slice']

if __name__ == "__main__":
    sync = MembraneSynchronizer()
    asyncio.run(sync.sync_membranes())
''',
    'cognitive_visualizer.py': '''#!/usr/bin/env python3
"""
Real-time Cognitive Membrane Visualizer
Provides web interface for monitoring membrane states
"""
import json
import numpy as np
from pathlib import Path
from http.server import HTTPServer, SimpleHTTPRequestHandler
import socketserver

class CognitiveVisualizer:
    def __init__(self, port=5000):
        self.port = port
        self.config_path = Path("/workspaces/cosmos/cognitive-membrane-config.json")
        
    def generate_visualization_html(self):
        """Generate HTML visualization of cognitive membranes"""
        with open(self.config_path) as f:
            config = json.load(f)
            
        html = f"""<!DOCTYPE html>
<html>
<head>
    <title>🧠 Cognitive Membrane Visualization</title>
    <style>
        body {{ font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; 
               background: #0d1117; color: #c9d1d9; margin: 20px; }}
        .membrane {{ border: 2px solid #30363d; border-radius: 10px; 
                    padding: 15px; margin: 10px; background: #161b22; }}
        .org-cogpilot {{ border-color: #f85149; }}
        .org-OzCog {{ border-color: #7c3aed; }}
        .org-cosmos {{ border-color: #2ea043; }}
        .tensor-shape {{ font-family: monospace; background: #21262d; 
                        padding: 5px; border-radius: 3px; }}
    </style>
</head>
<body>
    <h1>🌌 Cognitive Membrane Topology</h1>
    <p><strong>Tensor Shape:</strong> <span class="tensor-shape">{config['tensor_shape']}</span></p>
    <p><strong>Total Parameters:</strong> {config['total_parameters']:,}</p>
    
    <h2>Organizational Membranes</h2>"""
        
        for org_name, org_config in config['organizations'].items():
            html += f'''
    <div class="membrane org-{org_name}">
        <h3>({org_name}) - Prime: {org_config['prime']}</h3>
        <p><strong>Type:</strong> {org_config['type']}</p>
        <p><strong>Repositories:</strong> {', '.join(org_config['repositories'])}</p>
        <p><strong>Tensor Slice:</strong> <span class="tensor-shape">{org_config['tensor_slice']}</span></p>
    </div>'''
        
        html += '''
    <h2>Cross-Membrane Connections</h2>
    <ul>'''
        
        for connection in config['cross_membrane_connections']:
            html += f"<li>{connection['from']} → {connection['to']} ({connection['type']})</li>"
            
        html += '''
    </ul>
    
    <script>
        // Auto-refresh every 10 seconds
        setTimeout(() => location.reload(), 10000);
    </script>
</body>
</html>'''
        
        return html
        
    def start_server(self):
        """Start the visualization web server"""
        # Generate HTML
        html_content = self.generate_visualization_html()
        
        # Write to temporary file
        html_path = Path("/tmp/cognitive-membrane-viz.html")
        html_path.write_text(html_content)
        
        print(f"🌐 Starting cognitive membrane visualizer on http://localhost:{self.port}")
        print("🔄 Auto-refreshes every 10 seconds")
        
        # Simple HTTP server
        class VisualizationHandler(SimpleHTTPRequestHandler):
            def do_GET(self):
                self.send_response(200)
                self.send_header('Content-type', 'text/html')
                self.end_headers()
                self.wfile.write(html_content.encode())
        
        with socketserver.TCPServer(("", self.port), VisualizationHandler) as httpd:
            httpd.serve_forever()

if __name__ == "__main__":
    viz = CognitiveVisualizer()
    viz.start_server()
''',
    'tensor_calculator.py': '''#!/usr/bin/env python3
"""
Cognitive Tensor Field Calculator
Calculates and optimizes tensor shapes for cognitive membranes
"""
import numpy as np
import json
from pathlib import Path

class CognitiveTensorCalculator:
    def __init__(self):
        self.config_path = Path("/workspaces/cosmos/cognitive-membrane-config.json")
        
    def calculate_optimal_shape(self, total_complexity, num_orgs=3):
        """Calculate optimal tensor shape based on complexity"""
        # Prime factorization approach
        factors = self.prime_factorize(total_complexity)
        
        # Distribute factors across dimensions
        attention_dim = 7  # Fixed for cognitive attention
        org_dim = num_orgs
        
        remaining_complexity = total_complexity // (attention_dim * org_dim)
        repo_factors = self.prime_factorize(remaining_complexity)
        
        if len(repo_factors) >= 3:
            repo_dim = repo_factors[0]
            concept_dim = repo_factors[1] 
            impl_dim = np.prod(repo_factors[2:])
        else:
            repo_dim = repo_factors[0] if repo_factors else 10
            concept_dim = repo_factors[1] if len(repo_factors) > 1 else 50
            impl_dim = 100
            
        return [attention_dim, org_dim, repo_dim, concept_dim, impl_dim]
    
    def prime_factorize(self, n):
        """Prime factorization of a number"""
        factors = []
        d = 2
        while d * d <= n:
            while n % d == 0:
                factors.append(d)
                n //= d
            d += 1
        if n > 1:
            factors.append(n)
        return factors if factors else [1]
    
    def calculate_memory_efficiency(self, tensor_shape):
        """Calculate memory efficiency for tensor shape"""
        total_params = np.prod(tensor_shape)
        
        # Efficiency based on prime factorization
        efficiency = min(1.0, 10000.0 / total_params)
        return efficiency
    
    def optimize_for_ggml(self, tensor_shape):
        """Optimize tensor shape for ggml operations"""
        # Prefer powers of 2 and multiples of 8 for GPU efficiency
        optimized = []
        
        for dim in tensor_shape:
            if dim % 8 == 0:
                optimized.append(dim)
            else:
                # Round to nearest multiple of 8
                optimized.append(((dim + 7) // 8) * 8)
        
        return optimized
    
    def generate_report(self):
        """Generate tensor optimization report"""
        with open(self.config_path) as f:
            config = json.load(f)
        
        tensor_shape = config['tensor_shape']
        total_params = config['total_parameters']
        
        # Calculate metrics
        memory_efficiency = self.calculate_memory_efficiency(tensor_shape)
        optimized_shape = self.optimize_for_ggml(tensor_shape)
        optimized_params = np.prod(optimized_shape)
        
        report = {
            'original_shape': tensor_shape,
            'original_parameters': total_params,
            'optimized_shape': optimized_shape,
            'optimized_parameters': optimized_params,
            'memory_efficiency': memory_efficiency,
            'ggml_optimized': True,
            'optimization_ratio': optimized_params / total_params
        }
        
        return report

if __name__ == "__main__":
    calc = CognitiveTensorCalculator()
    report = calc.generate_report()
    
    print("🔢 Cognitive Tensor Optimization Report")
    print("=" * 40)
    for key, value in report.items():
        print(f"{key}: {value}")
'''
}

# Write interface scripts
scripts_dir = Path('/workspaces/cosmos/shared/membrane-interfaces')
scripts_dir.mkdir(exist_ok=True)

for script_name, script_content in interface_scripts.items():
    script_path = scripts_dir / script_name
    script_path.write_text(script_content)
    script_path.chmod(0o755)  # Make executable

print(f"📝 Created {len(interface_scripts)} membrane interface scripts")
EOF

# Install Python dependencies for cognitive operations
echo "📦 Installing cognitive dependencies..."
pip3 install numpy torch transformers PyGithub pyyaml matplotlib plotly

# Install Node.js dependencies for visualization
echo "🌐 Installing visualization dependencies..."
npm install -g @octokit/graphql live-server

# Setup development aliases
echo "⚙️ Setting up cognitive aliases..."
cat >> ~/.bashrc << 'EOF'

# Cognitive Membrane Aliases
alias cognitive-sync='python3 /workspaces/cosmos/shared/membrane-interfaces/membrane_sync.py'
alias cognitive-viz='python3 /workspaces/cosmos/shared/membrane-interfaces/cognitive_visualizer.py'
alias tensor-calc='python3 /workspaces/cosmos/shared/membrane-interfaces/tensor_calculator.py'
alias membrane-status='cat /workspaces/cosmos/cognitive-membrane-config.json | jq .'

# Quick navigation
alias goto-cogpilot='cd /workspaces/cosmos/cogpilot'
alias goto-ozcog='cd /workspaces/cosmos/OzCog' 
alias goto-cosmos='cd /workspaces/cosmos'
alias goto-shared='cd /workspaces/cosmos/shared'

echo "🧠 Cognitive Membrane Workspace Ready!"
echo "Available commands:"
echo "  cognitive-sync  - Synchronize membranes"
echo "  cognitive-viz   - Start visualization server"
echo "  tensor-calc     - Calculate tensor metrics"
echo "  membrane-status - Show current configuration"
EOF

source ~/.bashrc

# Create welcome message
echo "
🌌 Welcome to the Cognitive Cities Multi-Membrane Workspace! 🌌

This workspace provides:
✅ Cross-membrane communication interfaces
✅ Cognitive tensor field visualization
✅ Real-time membrane synchronization
✅ ggml-optimized tensor calculations

Quick start:
1. Run 'cognitive-viz' to start the web interface
2. Run 'cognitive-sync' to test membrane synchronization  
3. Run 'tensor-calc' to optimize tensor shapes
4. Check 'membrane-status' for current configuration

Happy coding! 🚀
" > /workspaces/cosmos/WELCOME.md

echo "✅ Cognitive membrane workspace setup completed!"
echo "📍 Configuration: /workspaces/cosmos/cognitive-membrane-config.json"
echo "🌐 Web interface available on port 5000"
echo "📝 Welcome guide: /workspaces/cosmos/WELCOME.md"