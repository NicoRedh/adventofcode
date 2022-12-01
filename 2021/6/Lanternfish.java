import java.io.BufferedReader;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.List;

public class Lanternfish {
    public static ArrayList<Integer> readFish(String filename) {
        ArrayList<Integer> fish = new ArrayList<Integer>();
        try {
            FileReader reader = new FileReader(filename);
            try (BufferedReader buffReader = new BufferedReader(reader)) {
                String[] line = buffReader.readLine().split(",");
                for (String fishy : line) {
                    fish.add(Integer.parseInt(fishy));
                }
            }
        } catch (Exception e) {
            System.out.println("ERROR: file not found \n" + e);
        }

        return fish;
    }

    public static void main(String[] args) {
        List<Integer> fishy = readFish("input.txt");
        Fishies fish = new Fishies(fishy);
        for (int i = 0; i<256; i++){
            fish.simDay();
            System.out.println(fish.countFish());
        }
        System.out.println(fish.countFish());

    }
}