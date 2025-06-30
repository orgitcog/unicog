/**
 * cognitive-visualization.js
 * 
 * Phase II.4: Interactive Cognitive Visualization
 * Real-time visualization with recursive attention feedback
 */

class CognitiveVisualization {
    constructor() {
        this.canvas = document.getElementById('cognitiveCanvas');
        this.ctx = this.canvas.getContext('2d');
        
        // Visualization state
        this.nodes = new Map();
        this.edges = new Map();
        this.agents = new Map();
        this.patterns = new Map();
        this.attentionOverlays = new Map();
        
        // Recursive feedback state
        this.feedbackActive = false;
        this.feedbackStrength = 0.0;
        this.cycleCount = 0;
        this.adaptationRate = 0.1;
        
        // Rendering parameters
        this.zoom = 1.0;
        this.panX = 0;
        this.panY = 0;
        this.realTimeMode = false;
        this.updateRate = 30; // Hz
        
        // Attention parameters
        this.attentionThreshold = 0.5;
        this.maxAttentionRadius = 100;
        
        this.initializeCanvas();
        this.setupEventHandlers();
        this.generateSampleData();
        this.startVisualization();
    }

    initializeCanvas() {
        const resizeCanvas = () => {
            const rect = this.canvas.parentElement.getBoundingClientRect();
            this.canvas.width = rect.width;
            this.canvas.height = rect.height;
        };
        
        resizeCanvas();
        window.addEventListener('resize', resizeCanvas);
    }

    setupEventHandlers() {
        // Control panel handlers
        document.getElementById('attentionThreshold').addEventListener('input', (e) => {
            this.attentionThreshold = parseFloat(e.target.value);
        });

        document.getElementById('updateRate').addEventListener('input', (e) => {
            this.updateRate = parseInt(e.target.value);
            document.getElementById('updateRateDisplay').textContent = `${this.updateRate} Hz`;
        });

        document.getElementById('startBtn').addEventListener('click', () => {
            this.startRealTimeMode();
        });

        document.getElementById('pauseBtn').addEventListener('click', () => {
            this.pauseRealTimeMode();
        });

        document.getElementById('resetBtn').addEventListener('click', () => {
            this.reset();
        });

        // Canvas interaction handlers
        this.canvas.addEventListener('click', (e) => {
            this.handleCanvasClick(e);
        });

        this.canvas.addEventListener('mousemove', (e) => {
            this.handleCanvasHover(e);
        });

        // Zoom and pan
        this.canvas.addEventListener('wheel', (e) => {
            e.preventDefault();
            this.handleZoom(e);
        });
    }

    generateSampleData() {
        // Generate sample cognitive nodes
        for (let i = 0; i < 20; i++) {
            const nodeId = `node_${i}`;
            this.nodes.set(nodeId, {
                id: nodeId,
                x: Math.random() * (this.canvas.width - 100) + 50,
                y: Math.random() * (this.canvas.height - 100) + 50,
                state: Math.random(),
                attention: Math.random(),
                type: ['concept', 'predicate', 'evaluation'][Math.floor(Math.random() * 3)]
            });
        }

        // Generate sample edges
        const nodeIds = Array.from(this.nodes.keys());
        for (let i = 0; i < 30; i++) {
            const edgeId = `edge_${i}`;
            const source = nodeIds[Math.floor(Math.random() * nodeIds.length)];
            const target = nodeIds[Math.floor(Math.random() * nodeIds.length)];
            if (source !== target) {
                this.edges.set(edgeId, {
                    id: edgeId,
                    source: source,
                    target: target,
                    weight: Math.random(),
                    type: 'inheritance'
                });
            }
        }

        // Generate sample agents
        for (let i = 0; i < 3; i++) {
            const agentId = `agent_${i}`;
            this.agents.set(agentId, {
                id: agentId,
                x: Math.random() * (this.canvas.width - 100) + 50,
                y: Math.random() * (this.canvas.height - 100) + 50,
                state: [Math.random(), Math.random(), Math.random()],
                active: true,
                frequency: 5 + Math.random() * 10
            });
        }

        this.updateInfoPanels();
    }

    startVisualization() {
        const animate = () => {
            this.update();
            this.render();
            
            if (this.realTimeMode) {
                setTimeout(() => requestAnimationFrame(animate), 1000 / this.updateRate);
            } else {
                requestAnimationFrame(animate);
            }
        };
        
        animate();
    }

    startRealTimeMode() {
        this.realTimeMode = true;
        this.feedbackActive = true;
        document.getElementById('recursiveFeedback').style.display = 'block';
        document.getElementById('statusIndicator').textContent = 'Real-time Mode Active';
    }

    pauseRealTimeMode() {
        this.realTimeMode = false;
        this.feedbackActive = false;
        document.getElementById('recursiveFeedback').style.display = 'none';
        document.getElementById('statusIndicator').textContent = 'Visualization Paused';
    }

    reset() {
        this.nodes.clear();
        this.edges.clear();
        this.agents.clear();
        this.patterns.clear();
        this.attentionOverlays.clear();
        
        this.feedbackStrength = 0.0;
        this.cycleCount = 0;
        
        this.generateSampleData();
        document.getElementById('statusIndicator').textContent = 'Visualization Reset';
    }

    update() {
        if (!this.realTimeMode) return;

        this.cycleCount++;

        // Simulate cognitive state evolution
        this.nodes.forEach((node) => {
            // Apply small random changes to simulate cognitive dynamics
            node.state += (Math.random() - 0.5) * 0.02;
            node.state = Math.max(0, Math.min(1, node.state));
            
            // Update attention based on state and feedback
            node.attention += (Math.random() - 0.5) * 0.05 + this.feedbackStrength * 0.1;
            node.attention = Math.max(0, Math.min(1, node.attention));
        });

        // Simulate agent state evolution
        this.agents.forEach((agent) => {
            agent.state = agent.state.map(s => {
                let newState = s + (Math.random() - 0.5) * 0.03;
                return Math.max(0, Math.min(1, newState));
            });
        });

        // Detect emergent patterns
        this.detectEmergentPatterns();

        // Update attention overlays
        this.updateAttentionOverlays();

        // Apply recursive feedback
        if (this.feedbackActive) {
            this.applyRecursiveFeedback();
        }

        this.updateInfoPanels();
    }

    detectEmergentPatterns() {
        // Simple pattern detection based on node clustering
        const clusters = this.findNodeClusters();
        
        clusters.forEach((cluster, index) => {
            if (cluster.length >= 3) {
                const patternId = `pattern_${index}`;
                this.patterns.set(patternId, {
                    id: patternId,
                    nodes: cluster,
                    strength: cluster.length / this.nodes.size,
                    type: 'cluster',
                    detected: Date.now()
                });
            }
        });

        // Remove old patterns
        const now = Date.now();
        this.patterns.forEach((pattern, id) => {
            if (now - pattern.detected > 5000) { // 5 seconds
                this.patterns.delete(id);
            }
        });
    }

    findNodeClusters() {
        const clusters = [];
        const visited = new Set();
        const clusterThreshold = 80; // Distance threshold for clustering

        this.nodes.forEach((node) => {
            if (visited.has(node.id)) return;

            const cluster = [node.id];
            visited.add(node.id);

            this.nodes.forEach((otherNode) => {
                if (visited.has(otherNode.id)) return;

                const distance = Math.sqrt(
                    Math.pow(node.x - otherNode.x, 2) + 
                    Math.pow(node.y - otherNode.y, 2)
                );

                if (distance < clusterThreshold) {
                    cluster.push(otherNode.id);
                    visited.add(otherNode.id);
                }
            });

            if (cluster.length > 1) {
                clusters.push(cluster);
            }
        });

        return clusters;
    }

    updateAttentionOverlays() {
        this.attentionOverlays.clear();

        this.nodes.forEach((node) => {
            if (node.attention > this.attentionThreshold) {
                this.attentionOverlays.set(node.id, {
                    x: node.x,
                    y: node.y,
                    radius: node.attention * this.maxAttentionRadius,
                    intensity: node.attention
                });
            }
        });
    }

    applyRecursiveFeedback() {
        // Calculate average attention level
        let totalAttention = 0;
        this.nodes.forEach(node => totalAttention += node.attention);
        const avgAttention = totalAttention / this.nodes.size;

        // Update feedback strength based on attention distribution
        this.feedbackStrength = avgAttention * this.adaptationRate;

        // Apply feedback to cognitive state
        this.nodes.forEach((node) => {
            // Nodes with high attention influence their neighbors
            if (node.attention > this.attentionThreshold) {
                this.edges.forEach((edge) => {
                    if (edge.source === node.id) {
                        const targetNode = this.nodes.get(edge.target);
                        if (targetNode) {
                            targetNode.attention += this.feedbackStrength * 0.1;
                            targetNode.attention = Math.min(1, targetNode.attention);
                        }
                    }
                });
            }
        });
    }

    render() {
        // Clear canvas
        this.ctx.clearRect(0, 0, this.canvas.width, this.canvas.height);

        // Apply transforms
        this.ctx.save();
        this.ctx.scale(this.zoom, this.zoom);
        this.ctx.translate(this.panX, this.panY);

        // Render attention overlays (background)
        this.renderAttentionOverlays();

        // Render edges
        this.renderEdges();

        // Render pattern highlights
        this.renderPatterns();

        // Render nodes
        this.renderNodes();

        // Render agents
        this.renderAgents();

        this.ctx.restore();
    }

    renderAttentionOverlays() {
        this.attentionOverlays.forEach((overlay) => {
            const gradient = this.ctx.createRadialGradient(
                overlay.x, overlay.y, 0,
                overlay.x, overlay.y, overlay.radius
            );
            gradient.addColorStop(0, `rgba(0, 255, 65, ${overlay.intensity * 0.3})`);
            gradient.addColorStop(1, 'rgba(0, 255, 65, 0)');

            this.ctx.fillStyle = gradient;
            this.ctx.beginPath();
            this.ctx.arc(overlay.x, overlay.y, overlay.radius, 0, 2 * Math.PI);
            this.ctx.fill();
        });
    }

    renderEdges() {
        this.edges.forEach((edge) => {
            const sourceNode = this.nodes.get(edge.source);
            const targetNode = this.nodes.get(edge.target);
            
            if (sourceNode && targetNode) {
                const alpha = edge.weight * 0.7;
                this.ctx.strokeStyle = `rgba(0, 255, 65, ${alpha})`;
                this.ctx.lineWidth = edge.weight * 3;
                this.ctx.beginPath();
                this.ctx.moveTo(sourceNode.x, sourceNode.y);
                this.ctx.lineTo(targetNode.x, targetNode.y);
                this.ctx.stroke();
            }
        });
    }

    renderNodes() {
        this.nodes.forEach((node) => {
            // Node base
            const radius = 8 + node.state * 12;
            const intensity = node.attention;
            
            this.ctx.fillStyle = `rgba(0, 255, 65, ${0.7 + intensity * 0.3})`;
            this.ctx.beginPath();
            this.ctx.arc(node.x, node.y, radius, 0, 2 * Math.PI);
            this.ctx.fill();

            // Node glow
            if (intensity > this.attentionThreshold) {
                this.ctx.shadowColor = '#00ff41';
                this.ctx.shadowBlur = intensity * 20;
                this.ctx.beginPath();
                this.ctx.arc(node.x, node.y, radius, 0, 2 * Math.PI);
                this.ctx.fill();
                this.ctx.shadowBlur = 0;
            }

            // Node label
            this.ctx.fillStyle = '#000';
            this.ctx.font = '8px monospace';
            this.ctx.textAlign = 'center';
            this.ctx.fillText(node.type[0].toUpperCase(), node.x, node.y + 2);
        });
    }

    renderAgents() {
        this.agents.forEach((agent) => {
            // Agent circle
            this.ctx.strokeStyle = '#ff6b35';
            this.ctx.lineWidth = 3;
            this.ctx.beginPath();
            this.ctx.arc(agent.x, agent.y, 15, 0, 2 * Math.PI);
            this.ctx.stroke();

            // Agent glow
            if (agent.active) {
                this.ctx.shadowColor = '#ff6b35';
                this.ctx.shadowBlur = 15;
                this.ctx.beginPath();
                this.ctx.arc(agent.x, agent.y, 15, 0, 2 * Math.PI);
                this.ctx.stroke();
                this.ctx.shadowBlur = 0;
            }

            // Agent ID
            this.ctx.fillStyle = '#ff6b35';
            this.ctx.font = '10px monospace';
            this.ctx.textAlign = 'center';
            this.ctx.fillText(agent.id.split('_')[1], agent.x, agent.y + 3);
        });
    }

    renderPatterns() {
        this.patterns.forEach((pattern) => {
            if (pattern.nodes.length < 2) return;

            // Calculate bounding box
            let minX = Infinity, minY = Infinity, maxX = -Infinity, maxY = -Infinity;
            
            pattern.nodes.forEach(nodeId => {
                const node = this.nodes.get(nodeId);
                if (node) {
                    minX = Math.min(minX, node.x - 20);
                    minY = Math.min(minY, node.y - 20);
                    maxX = Math.max(maxX, node.x + 20);
                    maxY = Math.max(maxY, node.y + 20);
                }
            });

            // Draw pattern highlight
            this.ctx.strokeStyle = `rgba(255, 255, 0, ${pattern.strength})`;
            this.ctx.lineWidth = 2;
            this.ctx.setLineDash([5, 5]);
            this.ctx.strokeRect(minX, minY, maxX - minX, maxY - minY);
            this.ctx.setLineDash([]);
        });
    }

    handleCanvasClick(event) {
        const rect = this.canvas.getBoundingClientRect();
        const x = (event.clientX - rect.left) / this.zoom - this.panX;
        const y = (event.clientY - rect.top) / this.zoom - this.panY;

        // Check for node clicks
        this.nodes.forEach((node) => {
            const distance = Math.sqrt(Math.pow(x - node.x, 2) + Math.pow(y - node.y, 2));
            if (distance < 20) {
                this.generateVisualizationFeedback('click', node.id, 1.0);
            }
        });

        // Check for agent clicks
        this.agents.forEach((agent) => {
            const distance = Math.sqrt(Math.pow(x - agent.x, 2) + Math.pow(y - agent.y, 2));
            if (distance < 20) {
                this.generateVisualizationFeedback('click', agent.id, 1.0);
            }
        });
    }

    handleCanvasHover(event) {
        const rect = this.canvas.getBoundingClientRect();
        const x = (event.clientX - rect.left) / this.zoom - this.panX;
        const y = (event.clientY - rect.top) / this.zoom - this.panY;

        // Apply hover feedback with lower intensity
        this.nodes.forEach((node) => {
            const distance = Math.sqrt(Math.pow(x - node.x, 2) + Math.pow(y - node.y, 2));
            if (distance < 30) {
                this.generateVisualizationFeedback('hover', node.id, 0.2);
            }
        });
    }

    handleZoom(event) {
        const zoomFactor = event.deltaY > 0 ? 0.9 : 1.1;
        this.zoom *= zoomFactor;
        this.zoom = Math.max(0.1, Math.min(3.0, this.zoom));
    }

    generateVisualizationFeedback(interactionType, targetId, strength) {
        if (!this.feedbackActive) return;

        // Create feedback signal
        const feedbackSignal = {
            type: interactionType,
            target: targetId,
            strength: strength,
            timestamp: Date.now()
        };

        // Apply feedback to target
        const targetNode = this.nodes.get(targetId);
        const targetAgent = this.agents.get(targetId);

        if (targetNode) {
            targetNode.attention += strength * 0.2;
            targetNode.attention = Math.min(1, targetNode.attention);
        }

        if (targetAgent) {
            targetAgent.state = targetAgent.state.map(s => Math.min(1, s + strength * 0.1));
        }

        // Update feedback strength
        this.feedbackStrength = Math.min(1, this.feedbackStrength + strength * 0.1);

        // Visual feedback
        document.getElementById('recursiveFeedback').style.animation = 'none';
        setTimeout(() => {
            document.getElementById('recursiveFeedback').style.animation = 'feedback-flash 0.5s ease-out';
        }, 10);
    }

    updateInfoPanels() {
        // Update counters
        document.getElementById('nodeCount').textContent = this.nodes.size;
        document.getElementById('edgeCount').textContent = this.edges.size;
        document.getElementById('agentCount').textContent = this.agents.size;
        document.getElementById('patternCount').textContent = this.patterns.size;

        // Update feedback info
        document.getElementById('feedbackStrength').textContent = this.feedbackStrength.toFixed(3);
        document.getElementById('cycleCount').textContent = this.cycleCount;
        document.getElementById('adaptationRate').textContent = this.adaptationRate.toFixed(2);

        // Update attention list
        const attentionList = document.getElementById('attentionList');
        attentionList.innerHTML = '';
        
        const sortedNodes = Array.from(this.nodes.values())
            .sort((a, b) => b.attention - a.attention)
            .slice(0, 5);

        sortedNodes.forEach(node => {
            const item = document.createElement('div');
            item.className = 'info-item';
            item.innerHTML = `${node.id}: ${node.attention.toFixed(2)}`;
            attentionList.appendChild(item);
        });

        // Update pattern list
        const patternList = document.getElementById('patternList');
        patternList.innerHTML = '';
        
        this.patterns.forEach(pattern => {
            const item = document.createElement('div');
            item.className = 'info-item';
            item.innerHTML = `${pattern.type}: ${pattern.nodes.length} nodes (${pattern.strength.toFixed(2)})`;
            patternList.appendChild(item);
        });

        // Update agent status
        const agentStatus = document.getElementById('agentStatus');
        agentStatus.innerHTML = '';
        
        this.agents.forEach(agent => {
            const item = document.createElement('div');
            item.className = 'info-item';
            const avgState = agent.state.reduce((a, b) => a + b, 0) / agent.state.length;
            item.innerHTML = `${agent.id}: ${avgState.toFixed(2)} @ ${agent.frequency.toFixed(1)}Hz`;
            agentStatus.appendChild(item);
        });
    }
}

// Initialize the visualization when the page loads
document.addEventListener('DOMContentLoaded', () => {
    window.cognitiveVisualization = new CognitiveVisualization();
});