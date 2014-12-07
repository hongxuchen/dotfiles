c = get_config()
app = c.InteractiveShellApp

c.TerminalIPythonApp.display_banner = False

# load_subconfig('ipython_config.py', profile='default')

c.PromptManager.in_template = r'[{color.LightCyan}\Y2{color.LightBlue}]{color.Green}|\#> '
c.PromptManager.in2_template = r'{color.Green}|{color.LightGreen}\D{color.Green}> '
#c.PromptManager.out_template = r"{color.Cyan}[\#{color.Green}]:  "
c.PromptManager.out_template = r"\#{color.LightBlue}: "

c.PromptManager.justify = True

c.InteractiveShell.separate_in = ''
c.InteractiveShell.separate_out = ''
c.InteractiveShell.separate_out2 = ''

c.TerminalInteractiveShell.quiet = True
c.TerminalInteractiveShell.term_title = True

c.PrefilterManager.multi_line_specials = True

import platform
plt = platform.system()
if plt == 'Darwin':
    c.AliasManager.user_aliases = [('l', 'ls -GF')]
elif plt == 'Linux':
    c.AliasManager.user_aliases = [('l', 'ls -F --color=tty')]
