import re
import basic


def validate_command(command):
    file_pattern = r'^-F\s([\w.-]+)(\.\w+)$'
    match = re.match(file_pattern, command)

    if not match:
        print("Invalid command: Command format is incorrect.")
        return ''

    filename = match.group(1)
    extension = match.group(2)

    if not filename:
        print("Invalid command: Filename not found")
        return ''

    if not extension:
        print("Invalid command: Filename must have an extension.")
        return ''
    elif extension != '.lambda':
        print("Invalid command: Only '.lambda' files are supported.")
        return ''
    else:
        return command


def read_from_file(txt):
    filename = txt.split('-F', 1).pop(1).replace(" ", "")

    try:  # validity checks before file reading
        file = open(f"{filename}", "r")
    except FileNotFoundError:
        print(f"{filename} not found!")
        return
    except Exception as err:
        print(f"Unexpected {err}=, {type(err)}=")
        return

    for line_number, line in enumerate(file, start=1):
        line = line.strip()
        result, error = basic.run(filename, line)
        if error:
            print(error.as_string().replace("line 1", f"line {line_number}"))
        elif result:
            print(result)
