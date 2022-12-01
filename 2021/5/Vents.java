import java.io.BufferedReader;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.List;

public class Vents {
    //returns vent lines in form of [x1,y1,x2,y2]
    public static List<int[]> readVentLines(String filename) {
        List<int[]> data = new ArrayList<int[]>();
        
        try {
            FileReader reader = new FileReader(filename);
            try (BufferedReader buffReader = new BufferedReader(reader)) {
                String line;
                String[] input = new String[5];
                while ((line = buffReader.readLine()) != null) {
                    int[] intInput = new int[4];
                    String newLine = line.replaceAll(" -> ", ",");
                    //System.out.println(newLine);
                    input = newLine.split(",");
                    for(int i = 0; i<input.length; i++){
                        intInput[i] = Integer.parseInt(input[i]);
                    }
                    data.add(intInput);
                }

            }
        } catch (Exception e) {
            System.out.println("ERROR: file not found \n" + e);
        }
        return data;
    }

    public static void main(String[] args) {
        List<int[]> coords = readVentLines("input.txt");
        Grid grid = new Grid(coords);
        grid.drawLines(coords);
        //System.out.println(grid.checkOverlaps());
        grid.drawDiagonals(coords);
        //grid.printGrid();
        System.out.println(grid.checkOverlaps());

    }
}
