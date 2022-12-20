import java.util.*;

public class Valve {
    private String name;
    private int flow;
    private List<String> neighbors;
    private Map<String, Integer> distances;
    public Valve (String name, int flow, List<String> neighbors) {
        this.flow = flow;
        this.name = name;
        this.neighbors = neighbors;
    }
    public int getFlow() {
        return flow;
    }
    public String getName() {
        return name;
    }
    public List<String> getNeighbors() {
        return neighbors;
    }
    public void setFlow(int flow) {
        this.flow = flow;
    }
    public void setName(String name) {
        this.name = name;
    }
    public void setNeighbors(List<String> neighbors) {
        this.neighbors = neighbors;
    }
    public Map<String, Integer> getDistances() {
        return distances;
    }
    public void setDistances(Map<String, Integer> distances) {
        this.distances = distances;
    }
    @Override
    public String toString() {
        return "\n" + name + " flow: " + flow + " neighbors: " + neighbors.toString() + "distances: " + distances.toString();
    }
}
