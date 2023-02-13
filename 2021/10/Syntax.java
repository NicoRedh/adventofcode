import java.io.*;
import java.util.*;

public class Syntax {
    public static List<Line> readFile(String filename) {
        List<Line> lines = new ArrayList<Line>();

        try (
                FileReader freader = new FileReader(filename);
                BufferedReader buffReader = new BufferedReader(freader);) {
            String line;
            while ((line = buffReader.readLine()) != null) {
                List<Integer> parens = new ArrayList<Integer>();
                // ) -> 3
                // ] -> 57
                // } -> 1197
                // > -> 25137
                String[] splitLine = line.split("");

                for (int i = 0; i < splitLine.length; i++) {
                    switch (splitLine[i]) {
                        case "(":
                            parens.add(3);
                            break;
                        case ")":
                            parens.add(-3);
                            break;
                        case "[":
                            parens.add(57);
                            break;
                        case "]":
                            parens.add(-57);
                            break;
                        case "{":
                            parens.add(1197);
                            break;
                        case "}":
                            parens.add(-1197);
                            break;
                        case "<":
                            parens.add(25137);
                            break;
                        case ">":
                            parens.add(-25137);
                            break;
                        default:
                            break;
                    }
                }
                lines.add(new Line(parens));

            }

        } catch (Exception e) {
            e.printStackTrace();
        }

        return lines;
    }

    public static void main(String[] args) {
        List<Line> lines = readFile("input.txt");
        System.out.println("lines.length: " + lines.size());
        // part1:
        int count = 0;
        //List<Integer> corruptedIndex = new ArrayList<Integer>();
        for (int i = 0; i < lines.size(); i++) {
            //System.out.println("checkCorrupt: " +  lines.get(i).checkCorrupt() );
            // if (lines.get(i).checkCorrupt() == 0) {
            //     continue;
            // }
            count += lines.get(i).checkCorrupt();
            //lines.remove(i);
            
        }
        System.out.println("part1: " + count);
        System.out.println(lines.size());
        List<Long> incompleteScore = new ArrayList<Long>();
        for (Line l : lines) {
            if(l.checkCorrupt() == 0)
            incompleteScore.add(l.checkIncomplete());
        }
        System.out.println("incomplete size: "+incompleteScore.size());
        Collections.sort(incompleteScore);
        for(Long in : incompleteScore){
            System.out.println("ordered List: "+ in);
        }
        //int index = Math.round(incompleteScore.size()/2);
        System.out.println(incompleteScore.get(25));
    }
}