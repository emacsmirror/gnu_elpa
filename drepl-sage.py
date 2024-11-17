"""SageMath interface for dREPL."""
# NOTE: This script is appended to drepl-ipython.py

from sage.misc.banner import banner
from sage.repl.configuration import SAGE_EXTENSION
from sage.repl.rich_output import get_display_manager
from sage.repl.rich_output.backend_ipython import BackendIPythonNotebook

mime_types["image/svg+xml"] = lambda s: s.encode() if isinstance(s, str) else s


@InteractiveShellABC.register
class SageRepl(Repl):
    display_text = Unicode(
        "latex",
        help="The text display format (plain or latex).",
    ).tag(config=True)

    def __init__(self, config):
        self.ps1 = "sage: "
        self.ps2 = "....: "
        self.ps3 = self.separate_in = ""
        self.show_banner = banner
        super().__init__(config)
        self.mime_size_limit = 2**20
        self.extension_manager.load_extension(SAGE_EXTENSION)
        dm = get_display_manager()
        dm.switch_backend(BackendIPythonNotebook(), shell=self)
        dm.preferences.text = "latex"
        dm.preferences.graphics = "vector"
