/*
 * CognitiveAgent.cs
 *
 * Unity3D Cognitive Agent Script
 * Integrates Unity3D GameObjects with OpenCog cognitive systems
 */

using System;
using System.Collections.Generic;
using UnityEngine;
using System.Net.Sockets;
using System.Text;
using System.Threading;

namespace OpenCog.Unity3D
{
    public class CognitiveAgent : MonoBehaviour
    {
        [Header("Cognitive Configuration")]
        public string agentId = "unity3d_agent_001";
        public string cognitiveServerHost = "localhost";
        public int cognitiveServerPort = 8080;
        public float updateFrequency = 10.0f;
        
        [Header("Sensory Configuration")]
        public Camera[] sensorycameras;
        public AudioSource[] sensoryAudioSources;
        public Collider[] tactileSensors;
        
        [Header("Motor Configuration")]
        public float movementSpeed = 5.0f;
        public float rotationSpeed = 90.0f;
        public Animator characterAnimator;
        
        [Header("Debug")]
        public bool enableDebugLogging = true;
        public bool visualizeSensoryData = true;
        
        // Private members
        private TcpClient cognitiveClient;
        private NetworkStream cognitiveStream;
        private Thread communicationThread;
        private bool isConnected = false;
        private bool shouldStop = false;
        
        // Embodiment data
        private EmbodimentTensorData currentTensorData;
        private SensoryInputData currentSensoryData;
        private MotorCommandData currentMotorCommands;
        
        // Performance tracking
        private float lastUpdateTime;
        private int messagesSent = 0;
        private int messagesReceived = 0;
        
        void Start()
        {
            InitializeCognitiveAgent();
            ConnectToCognitiveServer();
        }
        
        void Update()
        {
            if (isConnected && Time.time - lastUpdateTime >= 1.0f / updateFrequency)
            {
                UpdateSensoryData();
                ProcessMotorCommands();
                SendCognitiveUpdate();
                lastUpdateTime = Time.time;
            }
        }
        
        void OnDestroy()
        {
            DisconnectFromCognitiveServer();
        }
        
        private void InitializeCognitiveAgent()
        {
            currentTensorData = new EmbodimentTensorData(agentId);
            currentSensoryData = new SensoryInputData();
            currentMotorCommands = new MotorCommandData();
            
            if (enableDebugLogging)
            {
                Debug.Log($"[CognitiveAgent] Initializing agent: {agentId}");
            }
        }
        
        private void ConnectToCognitiveServer()
        {
            try
            {
                cognitiveClient = new TcpClient(cognitiveServerHost, cognitiveServerPort);
                cognitiveStream = cognitiveClient.GetStream();
                isConnected = true;
                
                // Start communication thread
                communicationThread = new Thread(CommunicationWorker);
                communicationThread.Start();
                
                if (enableDebugLogging)
                {
                    Debug.Log($"[CognitiveAgent] Connected to cognitive server: {cognitiveServerHost}:{cognitiveServerPort}");
                }
            }
            catch (Exception e)
            {
                Debug.LogError($"[CognitiveAgent] Failed to connect to cognitive server: {e.Message}");
                isConnected = false;
            }
        }
        
        private void DisconnectFromCognitiveServer()
        {
            shouldStop = true;
            
            if (communicationThread != null && communicationThread.IsAlive)
            {
                communicationThread.Join(1000); // Wait up to 1 second
            }
            
            if (cognitiveStream != null)
            {
                cognitiveStream.Close();
            }
            
            if (cognitiveClient != null)
            {
                cognitiveClient.Close();
            }
            
            isConnected = false;
        }
        
        private void UpdateSensoryData()
        {
            currentSensoryData.timestamp = DateTime.Now;
            
            // Visual sensory data
            UpdateVisualSensoryData();
            
            // Audio sensory data
            UpdateAudioSensoryData();
            
            // Tactile/collision sensory data
            UpdateTactileSensoryData();
            
            // Proprioceptive data (position, rotation, movement)
            UpdateProprioceptiveSensoryData();
            
            // Update embodiment tensor based on sensory data
            UpdateEmbodimentTensor();
        }
        
        private void UpdateVisualSensoryData()
        {
            if (sensorycameras != null && sensorycameras.Length > 0)
            {
                Camera mainCamera = sensorycameras[0];
                
                // Raycast to detect visible objects
                RaycastHit[] hits = Physics.RaycastAll(
                    mainCamera.transform.position, 
                    mainCamera.transform.forward, 
                    100.0f
                );
                
                currentSensoryData.visibleObjects.Clear();
                foreach (var hit in hits)
                {
                    if (hit.collider.gameObject != this.gameObject)
                    {
                        VisibleObject obj = new VisibleObject
                        {
                            objectId = hit.collider.gameObject.name,
                            distance = hit.distance,
                            position = hit.point,
                            normal = hit.normal
                        };
                        currentSensoryData.visibleObjects.Add(obj);
                    }
                }
                
                // Light level detection
                currentSensoryData.lightLevel = RenderSettings.ambientIntensity;
            }
        }
        
        private void UpdateAudioSensoryData()
        {
            // Simplified audio processing
            currentSensoryData.audioSources.Clear();
            
            AudioSource[] allAudioSources = FindObjectsOfType<AudioSource>();
            foreach (var audioSource in allAudioSources)
            {
                if (audioSource.isPlaying)
                {
                    float distance = Vector3.Distance(transform.position, audioSource.transform.position);
                    AudioSourceData sourceData = new AudioSourceData
                    {
                        sourceId = audioSource.gameObject.name,
                        volume = audioSource.volume,
                        distance = distance,
                        position = audioSource.transform.position
                    };
                    currentSensoryData.audioSources.Add(sourceData);
                }
            }
        }
        
        private void UpdateTactileSensoryData()
        {
            // Check for collisions with tactile sensors
            currentSensoryData.contactPoints.Clear();
            
            if (tactileSensors != null)
            {
                foreach (var sensor in tactileSensors)
                {
                    Collider[] overlapping = Physics.OverlapBox(
                        sensor.bounds.center,
                        sensor.bounds.extents,
                        sensor.transform.rotation
                    );
                    
                    foreach (var overlap in overlapping)
                    {
                        if (overlap != sensor && overlap.gameObject != this.gameObject)
                        {
                            ContactPoint contact = new ContactPoint
                            {
                                contactId = overlap.gameObject.name,
                                position = overlap.ClosestPoint(sensor.bounds.center),
                                normal = (sensor.bounds.center - overlap.bounds.center).normalized
                            };
                            currentSensoryData.contactPoints.Add(contact);
                        }
                    }
                }
            }
        }
        
        private void UpdateProprioceptiveSensoryData()
        {
            currentSensoryData.agentPosition = transform.position;
            currentSensoryData.agentRotation = transform.rotation;
            
            // Velocity estimation
            Rigidbody rb = GetComponent<Rigidbody>();
            if (rb != null)
            {
                currentSensoryData.velocity = rb.velocity;
                currentSensoryData.angularVelocity = rb.angularVelocity;
            }
            else
            {
                currentSensoryData.velocity = Vector3.zero;
                currentSensoryData.angularVelocity = Vector3.zero;
            }
        }
        
        private void UpdateEmbodimentTensor()
        {
            // Update sensory modalities
            currentTensorData.sensoryModality[0] = Mathf.Clamp01(currentSensoryData.visibleObjects.Count / 10.0f); // Visual
            currentTensorData.sensoryModality[1] = Mathf.Clamp01(currentSensoryData.audioSources.Count / 5.0f);   // Audio
            currentTensorData.sensoryModality[2] = Mathf.Clamp01(currentSensoryData.contactPoints.Count / 3.0f); // Tactile
            currentTensorData.sensoryModality[3] = Mathf.Clamp01(currentSensoryData.velocity.magnitude / 10.0f); // Proprioceptive
            
            // Update spatial coordinates
            currentTensorData.spatialCoordinates = new float[] {
                transform.position.x,
                transform.position.y,
                transform.position.z,
                transform.rotation.eulerAngles.y
            };
            
            // Update embodiment state (always physical in Unity3D)
            currentTensorData.embodimentState[0] = 0.0f; // Virtual
            currentTensorData.embodimentState[1] = 1.0f; // Physical
            currentTensorData.embodimentState[2] = 0.0f; // Hybrid
            
            // Update interaction mode based on current activity
            bool isMoving = currentSensoryData.velocity.magnitude > 0.1f;
            bool hasContacts = currentSensoryData.contactPoints.Count > 0;
            
            if (hasContacts || isMoving)
            {
                currentTensorData.interactionMode[0] = 0.0f; // Passive
                currentTensorData.interactionMode[1] = 1.0f; // Active
                currentTensorData.interactionMode[2] = 0.2f; // Adaptive
            }
            else
            {
                currentTensorData.interactionMode[0] = 1.0f; // Passive
                currentTensorData.interactionMode[1] = 0.0f; // Active
                currentTensorData.interactionMode[2] = 0.0f; // Adaptive
            }
        }
        
        private void ProcessMotorCommands()
        {
            if (currentMotorCommands.HasMovementCommand())
            {
                ExecuteMovementCommand();
            }
            
            if (currentMotorCommands.HasRotationCommand())
            {
                ExecuteRotationCommand();
            }
            
            if (currentMotorCommands.HasAnimationCommand())
            {
                ExecuteAnimationCommand();
            }
        }
        
        private void ExecuteMovementCommand()
        {
            Vector3 targetPosition = currentMotorCommands.targetPosition;
            Vector3 direction = (targetPosition - transform.position).normalized;
            
            transform.Translate(direction * movementSpeed * Time.deltaTime);
        }
        
        private void ExecuteRotationCommand()
        {
            Quaternion targetRotation = currentMotorCommands.targetRotation;
            transform.rotation = Quaternion.RotateTowards(
                transform.rotation, 
                targetRotation, 
                rotationSpeed * Time.deltaTime
            );
        }
        
        private void ExecuteAnimationCommand()
        {
            if (characterAnimator != null && !string.IsNullOrEmpty(currentMotorCommands.animationName))
            {
                characterAnimator.SetTrigger(currentMotorCommands.animationName);
            }
        }
        
        private void SendCognitiveUpdate()
        {
            if (!isConnected) return;
            
            try
            {
                string updateMessage = CreateCognitiveUpdateMessage();
                byte[] data = Encoding.UTF8.GetBytes(updateMessage);
                
                cognitiveStream.Write(data, 0, data.Length);
                messagesSent++;
                
                if (enableDebugLogging && messagesSent % 60 == 0) // Log every 60 messages
                {
                    Debug.Log($"[CognitiveAgent] Sent {messagesSent} messages to cognitive server");
                }
            }
            catch (Exception e)
            {
                Debug.LogError($"[CognitiveAgent] Failed to send cognitive update: {e.Message}");
                isConnected = false;
            }
        }
        
        private string CreateCognitiveUpdateMessage()
        {
            // Create JSON message with current state
            var message = new
            {
                type = "cognitive_state_update",
                agent_id = agentId,
                timestamp = DateTime.UtcNow.ToString("o"),
                embodiment_tensor = currentTensorData.ToArray(),
                sensory_data = new
                {
                    visible_objects = currentSensoryData.visibleObjects.Count,
                    audio_sources = currentSensoryData.audioSources.Count,
                    contact_points = currentSensoryData.contactPoints.Count,
                    position = new float[] { transform.position.x, transform.position.y, transform.position.z },
                    rotation = new float[] { transform.rotation.x, transform.rotation.y, transform.rotation.z, transform.rotation.w }
                },
                cognitive_load = CalculateCognitiveLoad()
            };
            
            return JsonUtility.ToJson(message);
        }
        
        private float CalculateCognitiveLoad()
        {
            float load = 0.0f;
            
            // Sensory load
            load += currentSensoryData.visibleObjects.Count * 0.1f;
            load += currentSensoryData.audioSources.Count * 0.15f;
            load += currentSensoryData.contactPoints.Count * 0.2f;
            
            // Movement load
            load += currentSensoryData.velocity.magnitude * 0.05f;
            
            return Mathf.Clamp01(load);
        }
        
        private void CommunicationWorker()
        {
            byte[] buffer = new byte[4096];
            
            while (!shouldStop && isConnected)
            {
                try
                {
                    if (cognitiveStream.DataAvailable)
                    {
                        int bytesRead = cognitiveStream.Read(buffer, 0, buffer.Length);
                        if (bytesRead > 0)
                        {
                            string message = Encoding.UTF8.GetString(buffer, 0, bytesRead);
                            ProcessCognitiveMessage(message);
                            messagesReceived++;
                        }
                    }
                    
                    Thread.Sleep(10); // Small delay to prevent busy waiting
                }
                catch (Exception e)
                {
                    Debug.LogError($"[CognitiveAgent] Communication error: {e.Message}");
                    isConnected = false;
                    break;
                }
            }
        }
        
        private void ProcessCognitiveMessage(string message)
        {
            try
            {
                // Parse motor commands from cognitive system
                // This is a simplified implementation
                if (message.Contains("motor_command"))
                {
                    // Parse and update motor commands
                    // Implementation would depend on the actual message format
                }
            }
            catch (Exception e)
            {
                Debug.LogError($"[CognitiveAgent] Failed to process cognitive message: {e.Message}");
            }
        }
        
        void OnGUI()
        {
            if (enableDebugLogging)
            {
                GUILayout.BeginArea(new Rect(10, 10, 300, 200));
                GUILayout.Label($"Cognitive Agent: {agentId}");
                GUILayout.Label($"Connected: {isConnected}");
                GUILayout.Label($"Messages Sent: {messagesSent}");
                GUILayout.Label($"Messages Received: {messagesReceived}");
                GUILayout.Label($"Cognitive Load: {CalculateCognitiveLoad():F3}");
                GUILayout.Label($"Visible Objects: {currentSensoryData.visibleObjects.Count}");
                GUILayout.Label($"Audio Sources: {currentSensoryData.audioSources.Count}");
                GUILayout.Label($"Contact Points: {currentSensoryData.contactPoints.Count}");
                GUILayout.EndArea();
            }
        }
    }
}

// Supporting data structures
[System.Serializable]
public class EmbodimentTensorData
{
    public string agentId;
    public float[] sensoryModality = new float[4];
    public float[] motorCommand = new float[3];
    public float[] spatialCoordinates = new float[4];
    public float[] temporalContext = new float[3];
    public float actionConfidence = 0.5f;
    public float[] embodimentState = new float[3];
    public float[] interactionMode = new float[3];
    public float[] feedbackLoop = new float[3];
    
    public EmbodimentTensorData(string id)
    {
        agentId = id;
        temporalContext[1] = 1.0f; // Present
        feedbackLoop[1] = 1.0f; // Closed
    }
    
    public float[] ToArray()
    {
        List<float> result = new List<float>();
        result.AddRange(sensoryModality);
        result.AddRange(motorCommand);
        result.AddRange(spatialCoordinates);
        result.AddRange(temporalContext);
        result.Add(actionConfidence);
        result.AddRange(embodimentState);
        result.AddRange(interactionMode);
        result.AddRange(feedbackLoop);
        return result.ToArray();
    }
}

[System.Serializable]
public class SensoryInputData
{
    public DateTime timestamp;
    public List<VisibleObject> visibleObjects = new List<VisibleObject>();
    public List<AudioSourceData> audioSources = new List<AudioSourceData>();
    public List<ContactPoint> contactPoints = new List<ContactPoint>();
    public Vector3 agentPosition;
    public Quaternion agentRotation;
    public Vector3 velocity;
    public Vector3 angularVelocity;
    public float lightLevel;
}

[System.Serializable]
public class VisibleObject
{
    public string objectId;
    public float distance;
    public Vector3 position;
    public Vector3 normal;
}

[System.Serializable]
public class AudioSourceData
{
    public string sourceId;
    public float volume;
    public float distance;
    public Vector3 position;
}

[System.Serializable]
public class ContactPoint
{
    public string contactId;
    public Vector3 position;
    public Vector3 normal;
}

[System.Serializable]
public class MotorCommandData
{
    public Vector3 targetPosition;
    public Quaternion targetRotation;
    public string animationName;
    public float movementSpeed;
    public float rotationSpeed;
    
    public bool HasMovementCommand()
    {
        return targetPosition != Vector3.zero;
    }
    
    public bool HasRotationCommand()
    {
        return targetRotation != Quaternion.identity;
    }
    
    public bool HasAnimationCommand()
    {
        return !string.IsNullOrEmpty(animationName);
    }
}