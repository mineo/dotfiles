c = get_config()
# Set to confirm when you try to exit IPython with an EOF (Control-D in Unix,
# Control-Z/Enter in Windows). By typing 'exit' or 'quit', you can force a
# direct exit without any confirmation.
c.InteractiveShell.confirm_exit = False
c.InteractiveShellApp.exec_lines = ["from __future__ import division"]
