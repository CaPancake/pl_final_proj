import basic
import file_handling

while True:
	text = input('basic > ')

	if str(text).__eq__("QUIT"):
		print("Interpreter function terminated")
		break

	elif not text.startswith('-F'):  # invoke interpreter interactive-mode
		result, error = basic.run('<stdin>', text)
		if error: print(error.as_string())
		elif result: print(result)

	elif file_handling.validate_command(text).__eq__(text):  # invoke interpreter script-mode
		file_handling.read_from_file(text)
