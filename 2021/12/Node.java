import java.util.*;

public class Node {
    private String name;
    private Set<String> pointers;
    private boolean isLowerCase;

    public Node(String name) {
        pointers = new TreeSet<String>();
        this.name = name;
        char[] lc = name.toCharArray();
        if(Character.isLowerCase(lc[0])){
            isLowerCase = true;
        } else{
            isLowerCase = false;
        }

    }
    public boolean isLowerCase() {
        return isLowerCase;
    }

    public String getName() {
        return name;
    }

    public Set<String> getPointers() {
        return pointers;
    }
    public void setPointers(Set<String> pointers) {
        this.pointers = pointers;
    }
    public void eliminateDeadEnds(List<Node> nodes){
        char[] splitter = name.toCharArray();
        if(Character.isLowerCase(splitter[0])){
            for (String point : pointers){
                char[] pointSplitter = point.toCharArray();
                if(Character.isLowerCase(pointSplitter[0])){
                    boolean nodeExists = false;
                    for(Node node : nodes){
                        if(node.getName() == point) nodeExists = true;
                    }
                    if(!nodeExists){
                        pointers.remove(point);
                    }
                }
            }
        }
    }
}
