import json

# input file
file_npm = open('npm-uniquePatterns.json', 'r')
# clearing the output before regenerating
open('npm.corpus', 'w').close()
# output file
output_file = open ('npm.corpus', 'a')

while True:
 
    line = file_npm.readline()
    # if line is empty
    # end of file is reached
    if not line:
        break
    # parsing the JSON string
    p = json.loads(line)
    print(p['pattern'], file=output_file)

output_file.close()
file_npm.close()
