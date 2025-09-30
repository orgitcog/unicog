/*
 * Permissions.h
 *
 * Copyright (C) 2025 OpenCog Foundation
 * All Rights Reserved
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#ifndef _OPENCOG_PERMISSIONS_H
#define _OPENCOG_PERMISSIONS_H

#include <string>
#include <set>
#include <map>
#include <memory>
#include <mutex>

namespace opencog
{

/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * Permission levels for AtomSpace access
 */
enum class Permission
{
    NONE = 0,           // No access
    READ = 1,           // Read-only access
    WRITE = 2,          // Write access (implies read)
    DELETE = 4,         // Delete access (implies read/write)
    ADMIN = 8,          // Administrative access (all permissions)
    EXECUTE = 16        // Execute permission for executable atoms
};

// Permission operators for combining permissions
inline Permission operator|(Permission a, Permission b)
{
    return static_cast<Permission>(static_cast<int>(a) | static_cast<int>(b));
}

inline Permission operator&(Permission a, Permission b)
{
    return static_cast<Permission>(static_cast<int>(a) & static_cast<int>(b));
}

inline bool hasPermission(Permission granted, Permission required)
{
    return (granted & required) == required;
}

/**
 * User/role identification
 */
class Principal
{
private:
    std::string _name;
    std::string _type;  // "user" or "role"
    
public:
    Principal(const std::string& name, const std::string& type = "user")
        : _name(name), _type(type) {}
    
    const std::string& getName() const { return _name; }
    const std::string& getType() const { return _type; }
    
    bool operator<(const Principal& other) const
    {
        if (_type != other._type) return _type < other._type;
        return _name < other._name;
    }
    
    bool operator==(const Principal& other) const
    {
        return _type == other._type && _name == other._name;
    }
};

/**
 * Permission context - represents the current security context
 */
class PermissionContext
{
private:
    Principal _principal;
    std::set<Principal> _roles;
    std::map<std::string, Permission> _resource_permissions;
    
public:
    PermissionContext(const Principal& principal)
        : _principal(principal) {}
    
    void addRole(const Principal& role) { _roles.insert(role); }
    void removeRole(const Principal& role) { _roles.erase(role); }
    bool hasRole(const Principal& role) const { return _roles.count(role) > 0; }
    
    void setResourcePermission(const std::string& resource, Permission perm)
    {
        _resource_permissions[resource] = perm;
    }
    
    Permission getResourcePermission(const std::string& resource) const
    {
        auto it = _resource_permissions.find(resource);
        if (it != _resource_permissions.end()) return it->second;
        return Permission::NONE;
    }
    
    const Principal& getPrincipal() const { return _principal; }
    const std::set<Principal>& getRoles() const { return _roles; }
};

/**
 * Permission manager - manages permissions for AtomSpaces
 */
class PermissionManager
{
private:
    // Map from principal to their default permissions
    std::map<Principal, Permission> _principal_permissions;
    
    // Map from resource (AtomSpace ID) to principal-specific permissions
    std::map<std::string, std::map<Principal, Permission>> _resource_permissions;
    
    // Active contexts
    std::map<std::string, std::shared_ptr<PermissionContext>> _contexts;
    
    // Thread safety
    mutable std::mutex _mutex;
    
    // Singleton instance
    static std::shared_ptr<PermissionManager> _instance;
    static std::mutex _instance_mutex;
    
    PermissionManager() {}
    
public:
    static std::shared_ptr<PermissionManager> getInstance()
    {
        std::lock_guard<std::mutex> lock(_instance_mutex);
        if (!_instance) {
            _instance = std::shared_ptr<PermissionManager>(new PermissionManager());
        }
        return _instance;
    }
    
    /**
     * Set default permissions for a principal
     */
    void setDefaultPermissions(const Principal& principal, Permission perms)
    {
        std::lock_guard<std::mutex> lock(_mutex);
        _principal_permissions[principal] = perms;
    }
    
    /**
     * Set permissions for a principal on a specific resource
     */
    void setResourcePermissions(const std::string& resource, 
                               const Principal& principal, 
                               Permission perms)
    {
        std::lock_guard<std::mutex> lock(_mutex);
        _resource_permissions[resource][principal] = perms;
    }
    
    /**
     * Check if a principal has specific permission on a resource
     */
    bool hasPermission(const Principal& principal,
                      const std::string& resource,
                      Permission required) const
    {
        std::lock_guard<std::mutex> lock(_mutex);
        
        // Check resource-specific permissions first
        auto res_it = _resource_permissions.find(resource);
        if (res_it != _resource_permissions.end()) {
            auto prin_it = res_it->second.find(principal);
            if (prin_it != res_it->second.end()) {
                return hasPermission(prin_it->second, required);
            }
        }
        
        // Fall back to default permissions
        auto def_it = _principal_permissions.find(principal);
        if (def_it != _principal_permissions.end()) {
            return hasPermission(def_it->second, required);
        }
        
        // No permissions found
        return false;
    }
    
    /**
     * Create a new permission context
     */
    std::shared_ptr<PermissionContext> createContext(const Principal& principal)
    {
        std::lock_guard<std::mutex> lock(_mutex);
        auto context = std::make_shared<PermissionContext>(principal);
        _contexts[principal.getName()] = context;
        return context;
    }
    
    /**
     * Get an existing permission context
     */
    std::shared_ptr<PermissionContext> getContext(const std::string& name) const
    {
        std::lock_guard<std::mutex> lock(_mutex);
        auto it = _contexts.find(name);
        if (it != _contexts.end()) return it->second;
        return nullptr;
    }
    
    /**
     * Remove a permission context
     */
    void removeContext(const std::string& name)
    {
        std::lock_guard<std::mutex> lock(_mutex);
        _contexts.erase(name);
    }
    
    /**
     * Apply a permission context to check permissions
     */
    bool checkPermissionWithContext(const std::shared_ptr<PermissionContext>& context,
                                   const std::string& resource,
                                   Permission required) const
    {
        if (!context) return false;
        
        // Check context-specific resource permissions
        Permission ctx_perm = context->getResourcePermission(resource);
        if (hasPermission(ctx_perm, required)) return true;
        
        // Check principal permissions
        if (hasPermission(context->getPrincipal(), resource, required)) return true;
        
        // Check role permissions
        for (const auto& role : context->getRoles()) {
            if (hasPermission(role, resource, required)) return true;
        }
        
        return false;
    }
};

/** @}*/
} // namespace opencog

#endif // _OPENCOG_PERMISSIONS_H