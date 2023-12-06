import java.io.BufferedReader;
import java.io.FileReader;
import java.util.*;

public class Main {
    public static List<Node> readFile(String filename) {
        List<Node> nodes = new ArrayList<Node>();
        try (
                FileReader freader = new FileReader(filename);
                BufferedReader buffReader = new BufferedReader(freader);) {
            String line;
            while ((line = buffReader.readLine()) != null) {
                String[] split = line.split("-");
                addNode(split[1], split[0], nodes);
                addNode(split[0], split[1], nodes);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }

        return nodes;
    }

    public static void addNode(String origin, String pointer, List<Node> nodes) {
        boolean found = false;
        for (Node node : nodes) {
            if (node.getName().equals(origin)) {
                Set<String> tmp = node.getPointers();
                tmp.add(pointer);
                node.setPointers(tmp);
                found = true;
            }
        }
        if (!found) {
            nodes.add(new Node(origin));
            Set<String> temp = new TreeSet<String>();
            temp.add(pointer);
            nodes.get(nodes.size() - 1).setPointers(temp);
        }
    }

    public static void main(String[] args) {
        List<Node> nodes = readFile("test3.txt");
        System.out.println();
        List<TreeNode<String>> tree = new ArrayList<TreeNode<String>>();
        int[] count2 = { 0 };
        buildTree(tree, nodes, 0, count2);
        // System.out.println(tree.size());
        System.out.println("count: " + count2[0]);
    }

    public static void buildTree(List<TreeNode<String>> tree, List<Node> nodes, int index, int[] count2) {
        // System.out.println("index: " +index);
        if (tree.isEmpty()) { // Baum ist leer, erstmal root bilden.
            tree.add(new TreeNode<String>("start"));
            for (Node node : nodes) {
                if (node.getName().equals("start")) {
                    buildTree(tree, nodes, index, count2);
                }
            }
        } else { // rekursiver Aufruf:
            for (Node parent : nodes) {
                if (parent.getName().equals(tree.get(index).name)) {
                    if (parent.getPointers().isEmpty())
                        return;
                    for (String point : parent.getPointers()) {
                        char[] lower = point.toCharArray();
                        if (Character.isLowerCase(lower[0])) {
                            TreeNode<String> elder = tree.get(index);
                            List<String> branch = new ArrayList<String>();
                            boolean twice = false;
                            // if (elder.name.equals(point)) {
                            //     branch.add(point);
                            // }
                             do{
                                char[] elderChar = elder.name.toCharArray();
                                if (Character.isLowerCase(elderChar[0])) {
                                    branch.add(elder.name);
                                }
                                
                            }while ((elder=elder.parent) != null);
                            for (String b : branch) {
                                if (!b.equals("start") || !b.equals("end")) {
                                    int freq;
                                    if ((freq=Collections.frequency(branch, b)) > 1) {
                                        twice = true;
                                        //System.out.println(freq);
                                    }
                                }

                            }

                            if ((!branch.contains(point) || !twice) && !point.equals("start")) {
                                tree.add(tree.get(index).addChild(point));
                                if (point.equals("end")) {
                                    count2[0]++;
                                    return;
                                }
                                buildTree(tree, nodes, tree.size() - 1, count2);
                                // System.out.println("size: " + tree.size());
                            }
                        } else {
                            tree.add(tree.get(index).addChild(point));
                            if (point.equals("end")) {
                                count2[0]++;
                                return;
                            }
                            buildTree(tree, nodes, tree.size() - 1, count2);
                            // System.out.println("size: " + tree.size());
                        }
                    }
                }
            }
        }
    }
}
