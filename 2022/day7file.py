class File:
    def __init__(self, name, size, parent, is_folder=False):
        self.name = name
        self.size = size
        self.parent = parent
        self.is_folder = is_folder
        self.children = []

    def __str__(self):
        return self.name
    
    def calcSize(self):
        if self.is_folder:
            return sum([f.calcSize() for f in self.children])
        else:
            return self.size
    
    def add_file(self, path):
        if path.startswith("dir "):
            name = path[4:]
            f = File(name, 0, self, True)
            self.children.append(f)
        else:
            # is of format size filename
            size, name = path.split(" ")
            size = int(size)
            f = File(name, size, self)
            self.children.append(f)

    def change_working_directory(self, path):
        if path == "..":
            wd = self.parent
        elif path == "/":
            wd = self
            while wd.parent:
                wd = wd.parent
        else:
            for f in self.children:
                if f.name == path:
                    if f.is_folder:
                        wd = f
                        break
                    else:
                        raise Exception("Not a folder")
        return wd

    def find_folders(self, max_size, compare):
        folders = []
        if compare(self.calcSize(), max_size):
            folders.append(self)
        for f in self.children:
            if f.is_folder:
                folders += f.find_folders(max_size, compare)
        return folders
