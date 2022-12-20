import java.io.BufferedReader;
import java.io.FileReader;
import java.util.*;

import javax.lang.model.util.ElementFilter;
import javax.swing.plaf.synth.SynthScrollPaneUI;

public class Main {
    public static List<String> readFile(String filename) {
        List<String> content = new ArrayList<String>();
        try (FileReader fReader = new FileReader(filename);
                BufferedReader buffReader = new BufferedReader(fReader);) {
            String line;
            while ((line = buffReader.readLine()) != null) {
                content.add(line);
            }
        } catch (Exception e) {
            e.printStackTrace();
        } finally {

        }
        return content;
    }

    public static Map<String, Integer> dijkstra(Valve valve, Map<String, Valve> valves) {
        Map<String, Integer> distMap = new TreeMap<>();
        Map<String, Integer> resMap = new TreeMap<>();
        for (Valve v : valves.values()) {
            distMap.put(v.getName(), Integer.MAX_VALUE);
            distMap.put(valve.getName(), 0);
        }
        while (!distMap.isEmpty()) {
            String minKey = "";
            for (String key : distMap.keySet()) {
                if (minKey.isBlank())
                    minKey = key;

                if (distMap.containsKey(minKey)) {
                    if (distMap.get(key) < distMap.get(minKey))
                        minKey = key;
                }
            }
            for (String n : valves.get(minKey).getNeighbors()) {
                if (distMap.containsKey(n)) {
                    if (distMap.get(n) > distMap.get(minKey) + 1)
                        distMap.put(n, distMap.get(minKey) + 1);
                }
            }
            resMap.put(minKey, distMap.get(minKey));
            distMap.remove(minKey);
        }
        return resMap;
    }

    public static int bound(int minute, int value, Valve valve, List<Valve> restValves) {
        int res = value;
        for (Valve v : restValves) {
            Map<String, Integer> distMap = valve.getDistances();
            if (30 - (distMap.get(v.getName()) + minute + 1) > 0) {
                res += (30 - (distMap.get(v.getName()) + minute + 1)) * v.getFlow();
            }
        }
        return res;
    }

    public static int boundedTSP(int minute, int maxValue, int value, List<Valve> route, Valve valve,
            List<Valve> valves) {
        // in the end it has nothing to do with TSP, and the bound is not used. :)
        List<Valve> newRoute = new ArrayList<>();
        for (Valve v : route) {
            newRoute.add(v);
        }
        newRoute.add(valve);
        int maxVal = maxValue;
        for (Valve v : valves) {
            int newVal;
            if ((30 - (valve.getDistances().get(v.getName()) + minute + 1)) >= 0) {
                newVal = value + (30 - (valve.getDistances().get(v.getName()) + minute + 1)) * v.getFlow();
            } else {
                maxVal = Math.max(maxVal, value);
                continue;
            }

            List<Valve> newValves = new ArrayList<>();
            for (Valve va : valves) {
                newValves.add(va);
            }
            newValves.remove(v);
            int thisValue = boundedTSP(minute + valve.getDistances().get(v.getName()) + 1, maxVal, newVal, newRoute, v,
                    newValves);
            maxVal = Math.max(maxVal, thisValue);
            // debug::
            List<String> routeString = new ArrayList<>();
            for (Valve s : newRoute) {
                routeString.add(s.getName());
            }
            List<String> restString = new ArrayList<>();
            for (Valve s : newValves) {
                restString.add(s.getName());
            }
            // System.out.printf("Route %s || %s || %s \tmaxValue=%d, value=%d\n",
            // routeString.toString(),v.getName(), restString.toString(), maxVal, value);
        }
        return Math.max(value, maxVal);
    }

    public static int boundedTSP1(int elfMinute, int eleMinute, int maxValue, int value, Valve elfValve, Valve eleValve,
            List<Valve> valves) {
        // in the end it has nothing to do with TSP, and the bound is not really useful. :)
        if (bound(Math.min(eleMinute, elfMinute), value, eleValve, valves) < maxValue && bound(Math.min(eleMinute, elfMinute), value, elfValve, valves) < maxValue) return maxValue;
        List<Valve[]> pairs = new ArrayList<>();
        for (int i = 0; i < valves.size(); i++) {
            for (int j = 0; j < valves.size(); j++) {
                if (i == j) continue;
                Valve[] pair = { valves.get(i), valves.get(j) };
                pairs.add(pair);
            }
        }
        int maxVal = maxValue;
        //System.out.printf("Elf: %s, Ele: %s, Elfmin: %d, Elemin: %d     \tvalue: %d \t maxValue: %d\n", elfValve.getName(), eleValve.getName(), elfMinute, eleMinute, value, maxValue);
        for (Valve[] pair : pairs) {
            Valve v = pair[0];
            Valve elev = pair[1];
            int newVal = value;

            List<Valve> newValves = new ArrayList<>();
            for (Valve va : valves) {
                newValves.add(va);
            }
            Boolean elf = false, ele = false;
            if ((26 - (elfValve.getDistances().get(v.getName()) + elfMinute + 1)) >= 0) {
                newVal += (26 - (elfValve.getDistances().get(v.getName()) + elfMinute + 1)) * v.getFlow();
                newValves.remove(v);
                //System.out.println("newValElf: "+ newVal);
                elf = true;
            }
            if ((26 - (eleValve.getDistances().get(elev.getName()) + eleMinute + 1)) >= 0) {
                newVal += (26 - (eleValve.getDistances().get(elev.getName()) + eleMinute + 1)) * elev.getFlow();
                newValves.remove(elev);
                //System.out.println("newValEle: "+ newVal);
                ele = true;
            }
            if (!elf && !ele){
                maxVal = Math.max(maxVal, value);
                continue;
            }
            maxVal = Math.max(maxVal, boundedTSP1(elfMinute + elfValve.getDistances().get(v.getName()) + 1, eleMinute + eleValve.getDistances().get(elev.getName()) + 1, maxVal, newVal, v, elev, newValves));

        }
        return Math.max(value, maxVal);
    }

    public static List<Valve> parse(List<String> input) {
        List<Valve> res = new ArrayList<Valve>();
        for (String line : input) {
            String[] words = line.split(" ");
            String name = words[1];
            int flow = Integer.parseInt(words[4].substring(5).replace(';', ' ').strip());
            List<String> neighbors = new ArrayList<>();
            for (int i = 9; i < words.length; i++) {
                neighbors.add(words[i].replace(',', ' ').strip());
            }
            res.add(new Valve(name, flow, neighbors));

        }
        return res;
    }

    public static void main(String[] args) {
        List<Valve> valveList = parse(readFile("../input.txt"));
        Map<String, Valve> valveMap = new TreeMap<>();
        for (Valve v : valveList) {
            valveMap.put(v.getName(), v);
        }
        for (Valve v : valveList) {
            Map<String, Integer> dMap = dijkstra(v, valveMap);
            v.setDistances(dMap);
        }
        Valve start = valveMap.get("AA");
        for (int i = valveList.size() - 1; i >= 0; i--) {
            if (valveList.get(i).getFlow() == 0)
                valveList.remove(i);
        }

        int result = boundedTSP(0, 0, 0, new ArrayList<Valve>(), start, valveList);
        int result2 = boundedTSP1(0, 0, 0, 0, start, start, valveList);
        System.out.printf("Part1: %d \nPart2: %d", result, result2);

    }
}