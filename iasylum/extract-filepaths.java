public static java.util.Collection<String> extractAllPaths(java.util.Collection<String> currentList, java.io.File fileOrDir) throws java.io.IOException {
    if (fileOrDir.exists()) {
        if (fileOrDir.isFile()) {
            currentList.add(fileOrDir.getAbsolutePath());
        } else if (fileOrDir.isDirectory()) {
            java.io.File[] files = fileOrDir.listFiles();

            if (files != null) {
                for (java.io.File file : files) {
                    extractAllPaths(currentList, file);
                }
            }
        }
    }

    return currentList;
}

public static java.util.Collection<String> extractAllPaths(String path) throws java.io.IOException {
    return extractAllPaths(new java.util.ArrayList<String>(100), new java.io.File(path));
}

return extractAllPaths(input).toArray();
