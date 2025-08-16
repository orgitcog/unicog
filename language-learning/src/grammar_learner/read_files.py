# language-learning/src/grammar_learner/read_files.py                   # 90129
import logging
import os


def check_dir(dir_path, create = False, verbose = 'none'):
    logger = logging.getLogger(__name__ + ".check_dir")
    if os.path.exists(dir_path):
        return True
    else:
        if create:
            os.makedirs(dir_path)
            return True
        # else:
        #     logger.critical(f'No directory {dir_path}')
        #     return False

        raise FileNotFoundError(f'No directory {dir_path}')


def check_dir_files(dir_path, verbose = 'none'):
    logger = logging.getLogger(__name__ + ".check_dir_files")
    files = []
    if dir_path[-1] != '/':
        path = dir_path + '/'
    else:
        path = dir_path
    if os.path.exists(dir_path):
        logger.info(f'Directory {path} exists.')
        for filename in os.listdir(dir_path):
            files.append(path + filename)
            logger.info(filename)
    else:
        # logger.critical(f'No directory {dir_path}')
        raise FileNotFoundError(f'No directory {dir_path}')
    return files


def check_mst_files(input_dir, verbose = 'none'):
    logger = logging.getLogger(__name__ + ".check_mst_files")
    if check_dir(input_dir, create = False, verbose = verbose):
        files = check_dir_files(input_dir, verbose = verbose)
        if len(files) > 0:
            logger.info(files)
            response = {'input files': files}
            for i, file in enumerate(files):
                if os.path.isfile(file):
                    logger.info('File #' + str(i) + f' {file} checked')
                else:
                    logger.critical('File #' + str(i) + f' {file} check failed')
            return files, response
        else:
            logger.critical(f'Input directory {input_dir} is empty')
            return [], {'check_mst_file_error': 'empty input directory'}
    else:
        logger.critical(f'No input directory {input_dir}')
        return [], {'check_mst_file_error': 'no input directory'}


def check_dict(file_path):          # Check if file is a valid dictionary file
    if not os.path.isfile(file_path):
        return False
    # Check if file has .dict extension
    if not file_path.endswith('.dict'):
        return False
    # Check if file is readable and not empty
    try:
        with open(file_path, 'r') as f:
            first_line = f.readline().strip()
            return len(first_line) > 0
    except:
        return False


def check_ull(file_path):           # Check if file is a valid ULL (parses) file
    if not os.path.isfile(file_path):
        return False
    # Check if file has expected extensions or is readable
    try:
        with open(file_path, 'r') as f:
            first_line = f.readline().strip()
            # Check if first line looks like a parse line (starts with digit)
            if first_line and first_line[0].isdigit():
                return True
            # Or check if it's a sentence line (not empty)
            return len(first_line) > 0
    except:
        return False


def check_corpus(input_dir, verbose = 'none'):  # Check if directory contains valid corpus files
    if not check_dir(input_dir, False, verbose):
        return False
    
    files = check_dir_files(input_dir, verbose)
    if len(files) == 0:
        return False
    
    # Check if at least one file is a valid ULL file
    valid_files = 0
    total_lines = 0
    
    for file in files:
        if check_ull(file):
            valid_files += 1
            try:
                with open(file, 'r') as f:
                    lines = f.read().splitlines()
                    total_lines += len([l for l in lines if l.strip()])
            except:
                continue
    
    # Return True if we have valid files with content
    return valid_files > 0 and total_lines > 0


def check_path(par, t = 'else', **kwargs):  # Check and resolve path based on type
    # default: t = 'else': kwargs[par] is file or dir ⇒ return path
    if 'module_path' not in kwargs:
        return None
    
    module_path = kwargs['module_path']
    
    if par not in kwargs:
        print(f'"{par}" not in kwargs: {kwargs}')
        return None
    
    path = kwargs[par]
    if len(path) == 0:
        path = module_path
    elif 'home' not in path and not path.startswith('/'):
        path = os.path.join(module_path, path)
    
    # Check path based on type
    if 'dir' in t:
        return path if check_dir(path, True, 'max') else None
    elif 'fil' in t:  # file
        return path if os.path.isfile(path) else None
    elif 'dic' in t:  # 'dict', '.dict'
        return path if check_dict(path) else None
    elif 'cor' in t:  # corpus: dir with file(s)
        return path if check_corpus(path) else None
    elif 'ull' in t:  # single corpus file
        return path if check_ull(path) else None
    else:
        # Default: check if path exists as file or directory
        return path if (check_dir(path, False, 'none') or os.path.isfile(path)) else None

# Notes

# 81219 @alex: logger
# 81231 cleanup
# 90128 check_path, stubs: check_dict, check_ull
# TODO: cleanup, check_ull, check corpus dir or/and single file
