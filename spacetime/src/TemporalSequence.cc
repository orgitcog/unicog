/**
 * TemporalSequence.cc
 *
 * Advanced temporal sequence processing algorithms
 * 
 * Copyright (C) 2024 OpenCog Unified
 */

#include <vector>
#include <algorithm>
#include <numeric>
#include <map>
#include <queue>
#include <stdexcept>
#include <cmath>

namespace opencog
{

// Temporal event structure
struct TemporalEvent {
    double timestamp;
    std::string event_type;
    double value;
    std::map<std::string, double> attributes;
    
    TemporalEvent(double ts, const std::string& type, double val = 0.0)
        : timestamp(ts), event_type(type), value(val) {}
};

// Temporal sequence class
class TemporalSequence {
private:
    std::vector<TemporalEvent> events;
    bool sorted = false;
    
    void ensureSorted() {
        if (!sorted) {
            std::sort(events.begin(), events.end(),
                [](const TemporalEvent& a, const TemporalEvent& b) {
                    return a.timestamp < b.timestamp;
                });
            sorted = true;
        }
    }
    
public:
    void addEvent(const TemporalEvent& event) {
        events.push_back(event);
        sorted = false;
    }
    
    void addEvent(double timestamp, const std::string& type, double value = 0.0) {
        events.emplace_back(timestamp, type, value);
        sorted = false;
    }
    
    size_t size() const { return events.size(); }
    
    const TemporalEvent& operator[](size_t idx) const {
        if (idx >= events.size()) {
            throw std::out_of_range("Index out of range");
        }
        return events[idx];
    }
    
    // Get events within time window
    std::vector<TemporalEvent> getEventsInWindow(double start_time, double end_time) {
        ensureSorted();
        std::vector<TemporalEvent> result;
        
        for (const auto& event : events) {
            if (event.timestamp >= start_time && event.timestamp <= end_time) {
                result.push_back(event);
            } else if (event.timestamp > end_time) {
                break;
            }
        }
        
        return result;
    }
    
    // Calculate event frequency
    double getEventFrequency(const std::string& event_type, double window_size) {
        ensureSorted();
        if (events.empty()) return 0.0;
        
        int count = std::count_if(events.begin(), events.end(),
            [&event_type](const TemporalEvent& e) {
                return e.event_type == event_type;
            });
        
        double duration = events.back().timestamp - events.front().timestamp;
        if (duration < window_size) duration = window_size;
        
        return count / duration;
    }
    
    // Find periodic patterns
    std::vector<double> findPeriodicPatterns(const std::string& event_type, 
                                            double min_period = 1.0,
                                            double max_period = 1000.0,
                                            int num_periods = 10) {
        ensureSorted();
        std::vector<double> timestamps;
        
        // Extract timestamps for specific event type
        for (const auto& event : events) {
            if (event.event_type == event_type) {
                timestamps.push_back(event.timestamp);
            }
        }
        
        if (timestamps.size() < 3) return {};
        
        // Calculate inter-event intervals
        std::vector<double> intervals;
        for (size_t i = 1; i < timestamps.size(); ++i) {
            intervals.push_back(timestamps[i] - timestamps[i-1]);
        }
        
        // Find most common periods using histogram
        std::map<int, int> period_histogram;
        double bin_size = (max_period - min_period) / num_periods;
        
        for (double interval : intervals) {
            if (interval >= min_period && interval <= max_period) {
                int bin = static_cast<int>((interval - min_period) / bin_size);
                period_histogram[bin]++;
            }
        }
        
        // Extract top periods
        std::vector<std::pair<double, int>> period_counts;
        for (const auto& [bin, count] : period_histogram) {
            double period = min_period + (bin + 0.5) * bin_size;
            period_counts.emplace_back(period, count);
        }
        
        std::sort(period_counts.begin(), period_counts.end(),
            [](const auto& a, const auto& b) { return a.second > b.second; });
        
        std::vector<double> result;
        for (size_t i = 0; i < std::min(size_t(3), period_counts.size()); ++i) {
            if (period_counts[i].second >= 2) {
                result.push_back(period_counts[i].first);
            }
        }
        
        return result;
    }
    
    // Detect anomalies using sliding window
    std::vector<size_t> detectAnomalies(double window_size, double threshold = 2.0) {
        ensureSorted();
        std::vector<size_t> anomalies;
        
        if (events.size() < 10) return anomalies;
        
        for (size_t i = 5; i < events.size() - 5; ++i) {
            double current_time = events[i].timestamp;
            
            // Count events in window around current event
            int count_before = 0, count_after = 0;
            
            for (size_t j = 0; j < events.size(); ++j) {
                double dt = events[j].timestamp - current_time;
                if (dt >= -window_size && dt < 0) count_before++;
                if (dt > 0 && dt <= window_size) count_after++;
            }
            
            // Check for sudden changes in event density
            double ratio = (count_after > 0) ? 
                static_cast<double>(count_before) / count_after : 
                static_cast<double>(count_before);
            
            if (ratio > threshold || ratio < 1.0 / threshold) {
                anomalies.push_back(i);
            }
        }
        
        return anomalies;
    }
    
    // Calculate temporal correlation between event types
    double calculateTemporalCorrelation(const std::string& type1, 
                                      const std::string& type2,
                                      double max_lag = 10.0) {
        ensureSorted();
        
        std::vector<double> times1, times2;
        for (const auto& event : events) {
            if (event.event_type == type1) times1.push_back(event.timestamp);
            if (event.event_type == type2) times2.push_back(event.timestamp);
        }
        
        if (times1.empty() || times2.empty()) return 0.0;
        
        // Calculate cross-correlation at different lags
        double max_correlation = 0.0;
        int num_lags = 20;
        
        for (int lag_idx = -num_lags; lag_idx <= num_lags; ++lag_idx) {
            double lag = max_lag * lag_idx / num_lags;
            int matches = 0;
            
            for (double t1 : times1) {
                for (double t2 : times2) {
                    if (std::abs((t2 - t1) - lag) < max_lag / num_lags) {
                        matches++;
                    }
                }
            }
            
            double correlation = static_cast<double>(matches) / 
                               std::sqrt(times1.size() * times2.size());
            max_correlation = std::max(max_correlation, correlation);
        }
        
        return max_correlation;
    }
    
    // Segment sequence into phases based on event density
    std::vector<std::pair<double, double>> segmentByDensity(double min_segment_size = 10.0) {
        ensureSorted();
        std::vector<std::pair<double, double>> segments;
        
        if (events.size() < 2) return segments;
        
        // Calculate local density using sliding window
        std::vector<double> densities;
        double window = min_segment_size;
        
        for (size_t i = 0; i < events.size(); ++i) {
            double t = events[i].timestamp;
            int count = 0;
            
            for (const auto& e : events) {
                if (e.timestamp >= t - window/2 && e.timestamp <= t + window/2) {
                    count++;
                }
            }
            
            densities.push_back(count / window);
        }
        
        // Find change points in density
        double current_start = events[0].timestamp;
        double avg_density = densities[0];
        int segment_count = 1;
        
        for (size_t i = 1; i < events.size(); ++i) {
            double new_avg = (avg_density * segment_count + densities[i]) / (segment_count + 1);
            
            // Check if density changed significantly
            if (std::abs(densities[i] - avg_density) > 0.5 * avg_density) {
                // End current segment
                segments.emplace_back(current_start, events[i-1].timestamp);
                
                // Start new segment
                current_start = events[i].timestamp;
                avg_density = densities[i];
                segment_count = 1;
            } else {
                avg_density = new_avg;
                segment_count++;
            }
        }
        
        // Add final segment
        segments.emplace_back(current_start, events.back().timestamp);
        
        return segments;
    }
    
    // Extract temporal motifs (repeated patterns)
    std::vector<std::vector<std::string>> extractMotifs(int min_length = 2, 
                                                       int min_occurrences = 2) {
        ensureSorted();
        std::map<std::vector<std::string>, int> motif_counts;
        
        // Generate all subsequences of given length
        for (int length = min_length; length <= std::min(10, static_cast<int>(events.size())/2); ++length) {
            for (size_t i = 0; i <= events.size() - length; ++i) {
                std::vector<std::string> motif;
                for (size_t j = i; j < i + length; ++j) {
                    motif.push_back(events[j].event_type);
                }
                motif_counts[motif]++;
            }
        }
        
        // Filter motifs by occurrence count
        std::vector<std::vector<std::string>> result;
        for (const auto& [motif, count] : motif_counts) {
            if (count >= min_occurrences) {
                result.push_back(motif);
            }
        }
        
        // Sort by frequency
        std::sort(result.begin(), result.end(),
            [&motif_counts](const auto& a, const auto& b) {
                return motif_counts[a] > motif_counts[b];
            });
        
        return result;
    }
};

// Global function implementations for temporal sequence analysis

// Calculate temporal entropy
double calculateTemporalEntropy(const std::vector<double>& timestamps, double window_size) {
    if (timestamps.size() < 2) return 0.0;
    
    std::vector<double> intervals;
    for (size_t i = 1; i < timestamps.size(); ++i) {
        intervals.push_back(timestamps[i] - timestamps[i-1]);
    }
    
    // Discretize intervals into bins
    int num_bins = std::max(5, static_cast<int>(std::sqrt(intervals.size())));
    double min_interval = *std::min_element(intervals.begin(), intervals.end());
    double max_interval = *std::max_element(intervals.begin(), intervals.end());
    double bin_size = (max_interval - min_interval) / num_bins;
    
    std::vector<int> histogram(num_bins, 0);
    for (double interval : intervals) {
        int bin = std::min(num_bins - 1, 
                          static_cast<int>((interval - min_interval) / bin_size));
        histogram[bin]++;
    }
    
    // Calculate entropy
    double entropy = 0.0;
    double total = intervals.size();
    
    for (int count : histogram) {
        if (count > 0) {
            double p = count / total;
            entropy -= p * std::log2(p);
        }
    }
    
    return entropy;
}

// Predict next event time using simple models
double predictNextEventTime(const std::vector<double>& timestamps) {
    if (timestamps.empty()) return 0.0;
    if (timestamps.size() == 1) return timestamps[0];
    
    // Use exponential smoothing for prediction
    double alpha = 0.3;  // Smoothing parameter
    double current_estimate = timestamps[1] - timestamps[0];
    
    for (size_t i = 2; i < timestamps.size(); ++i) {
        double interval = timestamps[i] - timestamps[i-1];
        current_estimate = alpha * interval + (1 - alpha) * current_estimate;
    }
    
    return timestamps.back() + current_estimate;
}

// Temporal clustering of events
std::vector<std::vector<size_t>> clusterEventsByTime(
    const std::vector<double>& timestamps, 
    double max_gap) {
    
    std::vector<std::vector<size_t>> clusters;
    if (timestamps.empty()) return clusters;
    
    std::vector<size_t> indices(timestamps.size());
    std::iota(indices.begin(), indices.end(), 0);
    
    // Sort indices by timestamp
    std::sort(indices.begin(), indices.end(),
        [&timestamps](size_t a, size_t b) {
            return timestamps[a] < timestamps[b];
        });
    
    // Form clusters
    std::vector<size_t> current_cluster;
    current_cluster.push_back(indices[0]);
    
    for (size_t i = 1; i < indices.size(); ++i) {
        if (timestamps[indices[i]] - timestamps[indices[i-1]] <= max_gap) {
            current_cluster.push_back(indices[i]);
        } else {
            clusters.push_back(current_cluster);
            current_cluster.clear();
            current_cluster.push_back(indices[i]);
        }
    }
    
    if (!current_cluster.empty()) {
        clusters.push_back(current_cluster);
    }
    
    return clusters;
}

} // namespace opencog