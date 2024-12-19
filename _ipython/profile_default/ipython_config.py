import os

c = get_config()
app = c.InteractiveShellApp

c.TerminalIPythonApp.display_banner = False

p = c.TerminalInteractiveShell.prompts_class

p.in_template = r"[{color.LightCyan}\Y2{color.LightBlue}]{color.Green}|\#> "
p.in2_template = r"{color.Green}|{color.LightGreen}\D{color.Green}> "
# p.out_template = r"{color.Cyan}[\#{color.Green}]:  "
p.out_template = r"\#{color.LightBlue}: "

p.justify = True

c.InteractiveShell.separate_in = ""
c.InteractiveShell.separate_out = ""
c.InteractiveShell.separate_out2 = ""

c.TerminalInteractiveShell.quiet = True
c.TerminalInteractiveShell.term_title = True

c.PrefilterManager.multi_line_specials = True

def is_in_nushell():
    shells = ["shell", "SHELL"]
    for s in shells:
        shell_env = os.getenv(s)
        if shell_env is not None and shell_env.endswith("nu"):
            return True
    return False

curdir = os.path.dirname(__file__)

is_zsh = os.getenv("ZSH_NAME") is not None
if is_in_nushell():
    nu_ipy = os.path.join(curdir, "nushell.ipy")
    c.InteractiveShellApp.exec_files = [nu_ipy]
