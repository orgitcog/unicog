#!/bin/bash
#
# Automation Script for Recursive TODO Resolution
#
# This script provides a convenient interface for the recursive TODO resolution system.
#

set -e

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${BLUE}🧠 OpenCog Unified Recursive TODO Resolution System${NC}"
echo -e "${BLUE}=====================================================${NC}"

case "${1:-status}" in
    "status")
        echo -e "${YELLOW}📊 Current TODO Resolution Status:${NC}"
        python scripts/recursive_todo_resolver.py --status
        ;;
    
    "next-batch"|"next")
        echo -e "${YELLOW}🎯 Processing next batch of highest-priority TODOs...${NC}"
        
        # Check if GitHub integration should be enabled
        GITHUB_FLAG=""
        if [ "$2" = "--no-github" ]; then
            GITHUB_FLAG="--no-github"
            echo -e "${BLUE}📝 GitHub integration disabled${NC}"
        elif [ -n "$GITHUB_TOKEN" ]; then
            echo -e "${GREEN}🚀 GitHub integration enabled${NC}"
        else
            echo -e "${YELLOW}⚠️  Set GITHUB_TOKEN environment variable to enable automatic issue creation${NC}"
        fi
        
        python scripts/recursive_todo_resolver.py --next-batch $GITHUB_FLAG
        
        # Show what was generated
        if [ -f "TODO_BATCH_$(python scripts/recursive_todo_resolver.py --status 2>/dev/null | grep "Current Iteration" | awk '{print $3-1}')_ISSUE.md" ]; then
            BATCH_NUM=$(($(python scripts/recursive_todo_resolver.py --status 2>/dev/null | grep "Current Iteration" | awk '{print $3}') - 1))
            echo -e "${GREEN}✅ Generated issue template: TODO_BATCH_${BATCH_NUM}_ISSUE.md${NC}"
            echo -e "${BLUE}📋 To create a GitHub issue:${NC}"
            echo -e "   1. Copy the content from TODO_BATCH_${BATCH_NUM}_ISSUE.md"
            echo -e "   2. Create a new GitHub issue with this content"
            echo -e "   3. Assign developers to work on the tasks"
            echo -e "   4. Use --mark-completed when tasks are done"
        fi
        ;;
    
    "mark-completed")
        if [ -z "$2" ] || [ -z "$3" ]; then
            echo -e "${RED}❌ Usage: $0 mark-completed FILE:LINE PR_LINK${NC}"
            echo -e "   Example: $0 mark-completed cogutil/opencog/util/Logger.cc:72 https://github.com/OzCog/opencog-unified/pull/123"
            exit 1
        fi
        
        echo -e "${YELLOW}✅ Marking TODO as completed: $2${NC}"
        python scripts/recursive_todo_resolver.py --mark-completed "$2" "$3"
        
        # Regenerate catalog to reflect changes
        echo -e "${YELLOW}📝 Updating catalog with completion...${NC}"
        python scripts/generate_todo_catalog.py
        python scripts/recursive_todo_resolver.py --status
        ;;
    
    "regenerate-catalog")
        echo -e "${YELLOW}🔄 Regenerating comprehensive TODO catalog...${NC}"
        python scripts/generate_todo_catalog.py
        echo -e "${GREEN}✅ Catalog regenerated${NC}"
        ;;
    
    "help"|"-h"|"--help")
        echo -e "${BLUE}Available commands:${NC}"
        echo -e "  status          - Show current TODO resolution status"
        echo -e "  next-batch      - Process next batch of highest-priority TODOs"
        echo -e "  mark-completed FILE:LINE PR_LINK - Mark a TODO as completed"
        echo -e "  regenerate-catalog - Regenerate the comprehensive TODO catalog"
        echo -e "  help            - Show this help message"
        echo
        echo -e "${BLUE}Examples:${NC}"
        echo -e "  $0 status"
        echo -e "  $0 next-batch"
        echo -e "  $0 mark-completed cogutil/opencog/util/Logger.cc:72 https://github.com/OzCog/opencog-unified/pull/123"
        ;;
    
    *)
        echo -e "${RED}❌ Unknown command: $1${NC}"
        echo -e "Use '$0 help' for available commands"
        exit 1
        ;;
esac

echo -e "${BLUE}=====================================================${NC}"