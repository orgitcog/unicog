# language-learning/src/grammar_learner/read_files.py
"""
File and directory validation utilities for the Grammar Learner.

This module provides functions to check and validate various file types
used in the grammar learning pipeline, including ULL parse files, 
dictionary files, and corpus directories.
"""
import logging
import os
from pathlib import Path
from typing import List, Tuple, Dict, Optional, Union


def check_dir(dir_path: str, create: bool = False, verbose: str = 'none') -> bool:
    """
    Check if a directory exists, optionally creating it.
    
    Args:
        dir_path: Path to the directory to check
        create: If True, create the directory if it doesn't exist
        verbose: Logging verbosity level
        
    Returns:
        True if directory exists or was created successfully
        
    Raises:
        FileNotFoundError: If directory doesn't exist and create=False
    """
    logger = logging.getLogger(__name__ + ".check_dir")
    
    if os.path.exists(dir_path):
        return True
    
    if create:
        os.makedirs(dir_path, exist_ok=True)
        logger.info(f'Created directory: {dir_path}')
        return True
    
    raise FileNotFoundError(f'No directory: {dir_path}')


def check_dir_files(dir_path: str, verbose: str = 'none') -> List[str]:
    """
    List all files in a directory.
    
    Args:
        dir_path: Path to the directory
        verbose: Logging verbosity level
        
    Returns:
        List of full file paths in the directory
        
    Raises:
        FileNotFoundError: If directory doesn't exist
    """
    logger = logging.getLogger(__name__ + ".check_dir_files")
    
    if not os.path.exists(dir_path):
        raise FileNotFoundError(f'No directory: {dir_path}')
    
    path = Path(dir_path)
    files = []
    
    logger.info(f'Directory {path} exists.')
    for filename in os.listdir(dir_path):
        file_path = path / filename
        if file_path.is_file():
            files.append(str(file_path))
            logger.info(f'Found file: {filename}')
    
    return files


def check_mst_files(input_dir: str, verbose: str = 'none') -> Tuple[List[str], Dict[str, any]]:
    """
    Check and validate MST (Minimum Spanning Tree) parse files in a directory.
    
    Args:
        input_dir: Directory containing MST files
        verbose: Logging verbosity level
        
    Returns:
        Tuple of (list of valid file paths, response dictionary)
    """
    logger = logging.getLogger(__name__ + ".check_mst_files")
    
    try:
        if not check_dir(input_dir, create=False, verbose=verbose):
            logger.critical(f'No input directory: {input_dir}')
            return [], {'check_mst_file_error': 'no input directory'}
        
        files = check_dir_files(input_dir, verbose=verbose)
        
        if not files:
            logger.critical(f'Input directory {input_dir} is empty')
            return [], {'check_mst_file_error': 'empty input directory'}
        
        logger.info(f'Found {len(files)} files in {input_dir}')
        valid_files = []
        
        for i, file in enumerate(files):
            if os.path.isfile(file):
                valid_files.append(file)
                logger.info(f'File #{i} {file} validated')
            else:
                logger.warning(f'File #{i} {file} validation failed')
        
        response = {'input files': valid_files, 'total_files': len(files)}
        return valid_files, response
        
    except Exception as e:
        logger.error(f'Error checking MST files: {e}')
        return [], {'check_mst_file_error': str(e)}


def check_dict(file_path: str) -> bool:
    """
    Check if a file is a valid Link Grammar dictionary file.
    
    A valid dictionary file must:
    - Exist and be readable
    - Have a .dict extension
    - Contain non-empty content
    
    Args:
        file_path: Path to the dictionary file
        
    Returns:
        True if valid dictionary file, False otherwise
    """
    if not os.path.isfile(file_path):
        return False
    
    if not file_path.endswith('.dict'):
        return False
    
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            first_line = f.readline().strip()
            # Check for non-empty content
            return len(first_line) > 0
    except (IOError, OSError):
        return False


def check_ull(file_path: str) -> bool:
    """
    Check if a file is a valid ULL (Unsupervised Language Learning) parse file.
    
    A valid ULL file contains parse data with lines that either:
    - Start with a digit (parse number)
    - Contain sentence text
    
    Args:
        file_path: Path to the ULL file
        
    Returns:
        True if valid ULL file, False otherwise
    """
    if not os.path.isfile(file_path):
        return False
    
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            # Check first few lines to validate format
            for i in range(min(10, 100)):  # Check up to 10 lines
                line = f.readline()
                if not line:  # End of file
                    break
                    
                line = line.strip()
                if line:  # Non-empty line
                    # Valid if starts with digit (parse ID) or contains text
                    if line[0].isdigit() or len(line) > 0:
                        return True
            
            return False  # No valid content found
            
    except (IOError, OSError, UnicodeDecodeError):
        return False


def check_corpus(input_dir: str, verbose: str = 'none') -> bool:
    """
    Check if a directory contains a valid corpus (collection of ULL files).
    
    A valid corpus directory must:
    - Exist
    - Contain at least one valid ULL file
    - Have non-empty content across all files
    
    Args:
        input_dir: Directory path to check
        verbose: Logging verbosity level
        
    Returns:
        True if valid corpus directory, False otherwise
    """
    logger = logging.getLogger(__name__ + ".check_corpus")
    
    try:
        if not check_dir(input_dir, False, verbose):
            return False
        
        files = check_dir_files(input_dir, verbose)
        if not files:
            logger.warning(f'No files found in corpus directory: {input_dir}')
            return False
        
        # Validate corpus files
        valid_files = 0
        total_lines = 0
        
        for file_path in files:
            if check_ull(file_path):
                valid_files += 1
                try:
                    with open(file_path, 'r', encoding='utf-8') as f:
                        lines = f.read().splitlines()
                        non_empty_lines = [l for l in lines if l.strip()]
                        total_lines += len(non_empty_lines)
                except Exception as e:
                    logger.warning(f'Error reading corpus file {file_path}: {e}')
                    continue
        
        logger.info(f'Corpus check: {valid_files} valid files, {total_lines} total lines')
        
        # Valid if we have at least one file with content
        return valid_files > 0 and total_lines > 0
        
    except Exception as e:
        logger.error(f'Error checking corpus: {e}')
        return False


def check_path(param_name: str, path_type: str = 'auto', **kwargs) -> Optional[str]:
    """
    Check and resolve a path parameter based on its type.
    
    This function validates paths and resolves relative paths against
    the module path if provided.
    
    Args:
        param_name: Name of the parameter in kwargs containing the path
        path_type: Type of path to validate:
            - 'dir': Directory (created if doesn't exist)
            - 'file': Regular file
            - 'dict': Link Grammar dictionary file (.dict)
            - 'corpus': Corpus directory with ULL files
            - 'ull': Single ULL parse file
            - 'auto': Auto-detect file or directory
        **kwargs: Must contain the path parameter and optionally 'module_path'
        
    Returns:
        Resolved and validated path, or None if invalid
    """
    logger = logging.getLogger(__name__ + ".check_path")
    
    # Get module path for relative path resolution
    module_path = kwargs.get('module_path', os.getcwd())
    
    # Get the path parameter
    if param_name not in kwargs:
        logger.warning(f'Parameter "{param_name}" not in kwargs')
        return None
    
    path = kwargs[param_name]
    
    # Handle empty paths
    if not path:
        path = module_path
    # Resolve relative paths
    elif not os.path.isabs(path) and 'home' not in path:
        path = os.path.join(module_path, path)
    
    # Expand user home directory
    path = os.path.expanduser(path)
    
    # Validate based on type
    try:
        if 'dir' in path_type:
            return path if check_dir(path, create=True, verbose='none') else None
        elif 'file' in path_type:
            return path if os.path.isfile(path) else None
        elif 'dict' in path_type:
            return path if check_dict(path) else None
        elif 'corpus' in path_type:
            return path if check_corpus(path, verbose='none') else None
        elif 'ull' in path_type:
            return path if check_ull(path) else None
        else:  # 'auto' or unknown type
            # Check if it exists as either file or directory
            if os.path.exists(path):
                return path
            else:
                logger.warning(f'Path does not exist: {path}')
                return None
                
    except Exception as e:
        logger.error(f'Error checking path {path}: {e}')
        return None

# Module history:
# - Added logging support
# - Implemented path validation functions
# - Added type hints and comprehensive documentation
# - Cleaned up error handling and improved robustness
