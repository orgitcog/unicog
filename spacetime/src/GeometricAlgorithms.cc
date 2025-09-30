/**
 * GeometricAlgorithms.cc
 *
 * Advanced geometric algorithms and computations
 * 
 * Copyright (C) 2024 OpenCog Unified
 */

#include <cmath>
#include <vector>
#include <algorithm>
#include <stdexcept>

namespace opencog
{

// Basic volume calculations
double calculateVolume(double width, double height, double depth)
{
    if (width < 0 || height < 0 || depth < 0) {
        throw std::invalid_argument("Dimensions must be non-negative");
    }
    return width * height * depth;
}

double calculateSurfaceArea(double width, double height, double depth)
{
    if (width < 0 || height < 0 || depth < 0) {
        throw std::invalid_argument("Dimensions must be non-negative");
    }
    return 2 * (width * height + width * depth + height * depth);
}

// Sphere calculations
double calculateSphereVolume(double radius)
{
    if (radius < 0) {
        throw std::invalid_argument("Radius must be non-negative");
    }
    return (4.0 / 3.0) * M_PI * radius * radius * radius;
}

double calculateSphereSurfaceArea(double radius)
{
    if (radius < 0) {
        throw std::invalid_argument("Radius must be non-negative");
    }
    return 4.0 * M_PI * radius * radius;
}

// Cylinder calculations
double calculateCylinderVolume(double radius, double height)
{
    if (radius < 0 || height < 0) {
        throw std::invalid_argument("Dimensions must be non-negative");
    }
    return M_PI * radius * radius * height;
}

double calculateCylinderSurfaceArea(double radius, double height)
{
    if (radius < 0 || height < 0) {
        throw std::invalid_argument("Dimensions must be non-negative");
    }
    return 2 * M_PI * radius * (radius + height);
}

// Distance calculations
double euclideanDistance(const std::vector<double>& point1, const std::vector<double>& point2)
{
    if (point1.size() != point2.size()) {
        throw std::invalid_argument("Points must have same dimensionality");
    }
    
    double sum = 0.0;
    for (size_t i = 0; i < point1.size(); ++i) {
        double diff = point1[i] - point2[i];
        sum += diff * diff;
    }
    return std::sqrt(sum);
}

double manhattanDistance(const std::vector<double>& point1, const std::vector<double>& point2)
{
    if (point1.size() != point2.size()) {
        throw std::invalid_argument("Points must have same dimensionality");
    }
    
    double sum = 0.0;
    for (size_t i = 0; i < point1.size(); ++i) {
        sum += std::abs(point1[i] - point2[i]);
    }
    return sum;
}

// Angle calculations
double angleBetweenVectors(const std::vector<double>& vec1, const std::vector<double>& vec2)
{
    if (vec1.size() != vec2.size() || vec1.empty()) {
        throw std::invalid_argument("Vectors must have same non-zero dimensionality");
    }
    
    double dotProduct = 0.0;
    double mag1 = 0.0;
    double mag2 = 0.0;
    
    for (size_t i = 0; i < vec1.size(); ++i) {
        dotProduct += vec1[i] * vec2[i];
        mag1 += vec1[i] * vec1[i];
        mag2 += vec2[i] * vec2[i];
    }
    
    mag1 = std::sqrt(mag1);
    mag2 = std::sqrt(mag2);
    
    if (mag1 == 0.0 || mag2 == 0.0) {
        throw std::invalid_argument("Cannot compute angle with zero vector");
    }
    
    double cosAngle = dotProduct / (mag1 * mag2);
    // Clamp to [-1, 1] to handle numerical errors
    cosAngle = std::max(-1.0, std::min(1.0, cosAngle));
    
    return std::acos(cosAngle);
}

// Polygon area calculation (2D)
double calculatePolygonArea(const std::vector<std::pair<double, double>>& vertices)
{
    if (vertices.size() < 3) {
        throw std::invalid_argument("Polygon must have at least 3 vertices");
    }
    
    double area = 0.0;
    size_t n = vertices.size();
    
    // Shoelace formula
    for (size_t i = 0; i < n; ++i) {
        size_t j = (i + 1) % n;
        area += vertices[i].first * vertices[j].second;
        area -= vertices[j].first * vertices[i].second;
    }
    
    return std::abs(area) / 2.0;
}

// Centroid calculation
std::vector<double> calculateCentroid(const std::vector<std::vector<double>>& points)
{
    if (points.empty()) {
        throw std::invalid_argument("Cannot compute centroid of empty point set");
    }
    
    size_t dimensions = points[0].size();
    std::vector<double> centroid(dimensions, 0.0);
    
    for (const auto& point : points) {
        if (point.size() != dimensions) {
            throw std::invalid_argument("All points must have same dimensionality");
        }
        for (size_t i = 0; i < dimensions; ++i) {
            centroid[i] += point[i];
        }
    }
    
    for (size_t i = 0; i < dimensions; ++i) {
        centroid[i] /= points.size();
    }
    
    return centroid;
}

// Bounding box calculation
std::pair<std::vector<double>, std::vector<double>> calculateBoundingBox(
    const std::vector<std::vector<double>>& points)
{
    if (points.empty()) {
        throw std::invalid_argument("Cannot compute bounding box of empty point set");
    }
    
    size_t dimensions = points[0].size();
    std::vector<double> minCorner(dimensions);
    std::vector<double> maxCorner(dimensions);
    
    // Initialize with first point
    for (size_t i = 0; i < dimensions; ++i) {
        minCorner[i] = maxCorner[i] = points[0][i];
    }
    
    // Find min and max for each dimension
    for (const auto& point : points) {
        if (point.size() != dimensions) {
            throw std::invalid_argument("All points must have same dimensionality");
        }
        for (size_t i = 0; i < dimensions; ++i) {
            minCorner[i] = std::min(minCorner[i], point[i]);
            maxCorner[i] = std::max(maxCorner[i], point[i]);
        }
    }
    
    return {minCorner, maxCorner};
}

// Point-in-polygon test (2D)
bool isPointInPolygon(const std::pair<double, double>& point,
                      const std::vector<std::pair<double, double>>& polygon)
{
    if (polygon.size() < 3) {
        throw std::invalid_argument("Polygon must have at least 3 vertices");
    }
    
    int n = polygon.size();
    bool inside = false;
    
    // Ray casting algorithm
    for (int i = 0, j = n - 1; i < n; j = i++) {
        double xi = polygon[i].first, yi = polygon[i].second;
        double xj = polygon[j].first, yj = polygon[j].second;
        
        if (((yi > point.second) != (yj > point.second)) &&
            (point.first < (xj - xi) * (point.second - yi) / (yj - yi) + xi)) {
            inside = !inside;
        }
    }
    
    return inside;
}

// Convex hull calculation (2D) using Graham scan
std::vector<std::pair<double, double>> convexHull(std::vector<std::pair<double, double>> points)
{
    if (points.size() < 3) {
        return points;
    }
    
    // Find the bottom-most point (or left-most in case of tie)
    auto pivot = std::min_element(points.begin(), points.end(),
        [](const auto& a, const auto& b) {
            return a.second < b.second || (a.second == b.second && a.first < b.first);
        });
    
    std::swap(*pivot, points[0]);
    auto p0 = points[0];
    
    // Sort by polar angle with respect to p0
    std::sort(points.begin() + 1, points.end(),
        [&p0](const auto& a, const auto& b) {
            double cross = (a.first - p0.first) * (b.second - p0.second) -
                          (b.first - p0.first) * (a.second - p0.second);
            if (cross == 0) {
                // If collinear, sort by distance
                double distA = (a.first - p0.first) * (a.first - p0.first) +
                              (a.second - p0.second) * (a.second - p0.second);
                double distB = (b.first - p0.first) * (b.first - p0.first) +
                              (b.second - p0.second) * (b.second - p0.second);
                return distA < distB;
            }
            return cross > 0;
        });
    
    // Build convex hull
    std::vector<std::pair<double, double>> hull;
    hull.push_back(p0);
    
    for (size_t i = 1; i < points.size(); ++i) {
        // Remove points that make clockwise turn
        while (hull.size() > 1) {
            auto& p1 = hull[hull.size() - 2];
            auto& p2 = hull[hull.size() - 1];
            auto& p3 = points[i];
            
            double cross = (p2.first - p1.first) * (p3.second - p1.second) -
                          (p3.first - p1.first) * (p2.second - p1.second);
            
            if (cross <= 0) {
                hull.pop_back();
            } else {
                break;
            }
        }
        hull.push_back(points[i]);
    }
    
    return hull;
}

} // namespace opencog