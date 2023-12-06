import java.util.LinkedList;
import java.util.List;

public class TreeNode<String> /*implements Iterable<TreeNode<String>> */{
    

    String name;
    TreeNode<String> parent;
    List<TreeNode<String>> children;

    public TreeNode(String name){
        this.name = name;
        this.children = new LinkedList<TreeNode<String>>();
    }

    public TreeNode<String> addChild(String name){
        TreeNode<String> childNode = new TreeNode<String>(name);
        childNode.parent = this;
        this.children.add(childNode);
        return childNode;
    }
}
