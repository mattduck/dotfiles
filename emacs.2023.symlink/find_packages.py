import pkgutil
import os

"""
Print a list of the locations of either python module directories, or .py files.

I can use this to quickly and reliably get a list of everything in the current environment
so I can narrow down to grep a particular package, or maybe to run eglot commands. I don't
know how to reliably get this list from the lsp server.
"""


def get_module_location(module_info):
    if hasattr(module_info.module_finder, 'path'):
        module_path = os.path.join(module_info.module_finder.path, module_info.name)
        if os.path.isdir(module_path):
            return module_path
        else:
            return module_path + ".py"
    return None


def main():
    for module_info in pkgutil.iter_modules():
        location = get_module_location(module_info)
        print(location)


if __name__ == "__main__":
    main()
