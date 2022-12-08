package com.chasingdings.y2022;

import java.util.ArrayList;
import java.util.List;
import java.util.function.BiFunction;

import org.apache.commons.lang3.StringUtils;

public class Puzzle7 extends AbstractPuzzle {
    private static final String DATA_FILE = "2022\\puzzle7.txt";

    private File root = null;

    @Override
    public Object solve1(String content) {
        makeFileSystem(content);

        var deleteMe = root.findFolders(100000, (a, b) -> a <= b);
        return deleteMe.stream().mapToInt(File::calcSize).sum();
    }

    @Override
    public Object solve2(String content) {
        makeFileSystem(content);

        var deleteMe = root.findFolders(root.calcSize() - 40000000, (a, b) -> a >= b);
        return deleteMe.stream().mapToInt(File::calcSize).min().getAsInt();
    }

    private void makeFileSystem(String content) {
        if (root == null) {
            root = new File("/", null);
            var wd = root;

            for (var line : getInputDataByLine(content)) {
                if (line.startsWith("$ cd ")) {
                    wd = wd.changeWorkingDirectory(line.substring(5));
                } else {
                    var toks = line.split(" ");
                    if (StringUtils.isNumeric(toks[0])) {
                        wd.addFile(toks[0]);
                    }
                }
            }
        }
    }

    @Override
    public String getDataFilePath() {
        return DATA_FILE;
    }

    @Override
    public String getPuzzleName() {
        return "Day 7 - No Space Left On Device";
    }

    /**
     * Only preserves directories, not files
     */
    class File {
        private String name;
        private int size;
        private File parent;
        private List<File> children;

        /**
         * This class represents a file or directory
         * 
         * @param name - name of the file
         * @param size - size of the file
         * @param parent - parent directory
         */
        public File(String name, File parent) {
            this.name = name;
            this.size = 0;
            this.parent = parent;
            this.children = new ArrayList<>();
        }

        /**
         * Calculates the size of the file and all of its children
         * 
         * @return the size of the file and all of its children
         */
        public int calcSize() {
            int fileSize = this.size;
            for (var f : this.children) {
                fileSize += f.calcSize();
            }
            return fileSize;
        }

        /*
         * Adds a file to the directory
         */
        public void addFile(String fileSize) {
            this.size += Integer.parseInt(fileSize);
        }

        /* 
         * Changes the working directory
         */
        public File changeWorkingDirectory(String path) {
            if (path.equals("..")) {
                return this.parent;
            } else if (path.equals("/")) {
                var wd = this;
                while (wd.parent != null) {
                    wd = wd.parent;
                }
                return wd;
            } else {
                for (var f : this.children) {
                    if (f.name.equals(path)) {
                        return f;
                    }
                }
                var newDir = new File(path, this);
                this.children.add(newDir);

                return newDir;
            }
        }

        /*
         * Finds all folders that match the condition set by compare
         */
        public List<File> findFolders(int maxSize, BiFunction<Integer, Integer, Boolean> compare) {
            var folders = new ArrayList<File>();
            if (compare.apply(this.calcSize(), maxSize)) {
                folders.add(this);
            }
            for (var f : this.children) {
                folders.addAll(f.findFolders(maxSize, compare));
            }
            return folders;
        }
    }
}
