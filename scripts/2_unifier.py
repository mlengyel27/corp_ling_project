#this program reads all the txt files from a given file location and combines them into one txt file

import os

def unifier(folder_path, unified_file_name):
    
    file_contents = []
    for filename in os.listdir(folder_path):
        # iterate through all the text files
        if filename.endswith('.txt'):
            file_path = os.path.join(folder_path, filename)

            # Read the contents of the txt files
            with open(file_path, 'r', encoding='utf-8') as file:
                file_contents.append(file.read())

    # join contents and write them into a file
    unified_contents = '\n'.join(file_contents)
    unified_file_path = os.path.join(folder_path, unified_file_name)
    with open(unified_file_path, 'w', encoding='utf-8') as unified_file:
        unified_file.write(unified_contents)

# set folder path and filename here
folder_path = r'\data\Trump'
unified_file_name = "Trump_unified.txt"
unifier(folder_path, unified_file_name)
