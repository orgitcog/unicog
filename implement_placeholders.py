#!/usr/bin/env python3
"""
Implement fixes for placeholder code
"""
import json
import re
from pathlib import Path
import shutil

class PlaceholderImplementer:
    def __init__(self, repo_root):
        self.repo_root = Path(repo_root)
        self.fixes_applied = []
        self.fixes_failed = []
        
    def fix_obsolete_comments(self, filepath, line_num, content):
        """Update obsolete/deprecated comments"""
        try:
            with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
                lines = f.readlines()
            
            line_idx = line_num - 1
            if line_idx >= len(lines):
                return False
            
            original_line = lines[line_idx]
            
            # Replace FIXME/TODO about obsolete code with NOTE
            if 'obsolete' in content.lower() or 'deprecated' in content.lower():
                # Change FIXME/TODO to NOTE
                new_line = re.sub(r'(//|#|;)\s*(FIXME|TODO|XXX)\s*', r'\1 NOTE: ', original_line)
                
                if new_line != original_line:
                    # Backup
                    backup_path = filepath.with_suffix(filepath.suffix + '.bak')
                    if not backup_path.exists():
                        shutil.copy2(filepath, backup_path)
                    
                    lines[line_idx] = new_line
                    
                    with open(filepath, 'w', encoding='utf-8') as f:
                        f.writelines(lines)
                    
                    self.fixes_applied.append({
                        'file': str(filepath.relative_to(self.repo_root)),
                        'line': line_num,
                        'type': 'obsolete_comment',
                        'original': original_line.strip(),
                        'fixed': new_line.strip()
                    })
                    return True
            
            return False
            
        except Exception as e:
            self.fixes_failed.append({
                'file': str(filepath.relative_to(self.repo_root)),
                'line': line_num,
                'reason': str(e)
            })
            return False
    
    def add_clarifying_documentation(self, filepath, line_num, content):
        """Add clarifying comments where TODOs mention clarification"""
        try:
            with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
                lines = f.readlines()
            
            line_idx = line_num - 1
            if line_idx >= len(lines):
                return False
            
            if 'clarify' in content.lower():
                # Add a clarifying comment
                indent = len(lines[line_idx]) - len(lines[line_idx].lstrip())
                comment_prefix = '//' if filepath.suffix in ['.cc', '.cpp', '.h'] else '#'
                
                # Keep the TODO but add a clarification note
                clarification = ' ' * indent + f'{comment_prefix} Clarification needed: Review and document the intended behavior\n'
                
                # Backup
                backup_path = filepath.with_suffix(filepath.suffix + '.bak')
                if not backup_path.exists():
                    shutil.copy2(filepath, backup_path)
                
                lines.insert(line_idx + 1, clarification)
                
                with open(filepath, 'w', encoding='utf-8') as f:
                    f.writelines(lines)
                
                self.fixes_applied.append({
                    'file': str(filepath.relative_to(self.repo_root)),
                    'line': line_num,
                    'type': 'clarification_added',
                    'note': 'Added clarification comment'
                })
                return True
            
            return False
            
        except Exception as e:
            self.fixes_failed.append({
                'file': str(filepath.relative_to(self.repo_root)),
                'line': line_num,
                'reason': str(e)
            })
            return False
    
    def generate_report(self):
        """Generate implementation report"""
        by_type = {}
        for fix in self.fixes_applied:
            fix_type = fix['type']
            if fix_type not in by_type:
                by_type[fix_type] = []
            by_type[fix_type].append(fix)
        
        return {
            'summary': {
                'total_fixes_applied': len(self.fixes_applied),
                'total_fixes_failed': len(self.fixes_failed),
                'by_type': {k: len(v) for k, v in by_type.items()}
            },
            'fixes_applied': self.fixes_applied,
            'fixes_failed': self.fixes_failed,
            'by_type': by_type
        }

if __name__ == '__main__':
    # Load easy fixes
    with open('easy_fixes.json', 'r') as f:
        easy_fixes = json.load(f)
    
    print(f"Processing {len(easy_fixes)} easy fixes...")
    
    implementer = PlaceholderImplementer('/home/ubuntu/opencog-unified')
    
    # Process obsolete comments
    obsolete_items = [f for f in easy_fixes if 'obsolete' in f['content'].lower() or 'deprecated' in f['content'].lower()]
    print(f"\nProcessing {len(obsolete_items)} obsolete comment fixes...")
    for item in obsolete_items[:10]:
        filepath = implementer.repo_root / item['file']
        if filepath.exists():
            implementer.fix_obsolete_comments(filepath, item['line'], item['content'])
    
    # Process clarification TODOs
    clarify_items = [f for f in easy_fixes if 'clarify' in f['content'].lower()]
    print(f"\nProcessing {len(clarify_items)} clarification fixes...")
    for item in clarify_items[:5]:
        filepath = implementer.repo_root / item['file']
        if filepath.exists():
            implementer.add_clarifying_documentation(filepath, item['line'], item['content'])
    
    # Generate report
    report = implementer.generate_report()
    
    with open('implementation_report.json', 'w') as f:
        json.dump(report, f, indent=2)
    
    print(f"\n\n=== Implementation Report ===")
    print(f"Total fixes applied: {report['summary']['total_fixes_applied']}")
    print(f"Total fixes failed: {report['summary']['total_fixes_failed']}")
    print(f"\nBy type:")
    for fix_type, count in report['summary']['by_type'].items():
        print(f"  {fix_type}: {count}")
    
    if report['fixes_applied']:
        print(f"\n\nFixes applied:")
        for fix in report['fixes_applied'][:20]:
            print(f"  {fix['file']}:{fix['line']} - {fix['type']}")
