import java.io.BufferedReader;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.List;

public class Power {
    public static List<String[]> readFile(String filename) {
        List<String[]> data = new ArrayList<String[]>();
        try {
            FileReader reader = new FileReader(filename);
            try (BufferedReader buffReader = new BufferedReader(reader)) {
                String line;
                int length = buffReader.readLine().split("").length;
                String[] input = new String[length];
                while ((line = buffReader.readLine()) != null) {
                    input = (line.split(""));
                    data.add(input);
                    // System.out.println(input.length);
                }
            }
        } catch (Exception e) {
            System.out.println("ERROR: file not found \n" + e);
        }
        return data;
    }

    public static void main(String[] args) {
        List<String[]> data = readFile(args[0]);
        System.out.println("binNumber Length: " + data.get(0).length);
        int[] zerocounts = new int[data.get(0).length];
        int[] onecounts = new int[data.get(0).length];
        for (int i = 0; i < data.size(); i++) {
            for (int j = 0; j < data.get(i).length; j++) {
                if (Integer.parseInt(data.get(i)[j]) == 0)
                    zerocounts[j]++;
                if (Integer.parseInt(data.get(i)[j]) == 1)
                    onecounts[j]++;
            }
        }
        System.out.println("zerocounts: " + zerocounts[0]);
        int[] finalGamma = new int[data.get(0).length];
        int[] finalEpsilon = new int[data.get(0).length];
        for (int i = 0; i < zerocounts.length; i++) {
            if (zerocounts[i] > onecounts[i]) {
                finalGamma[i] = 0;
                finalEpsilon[i] = 1;
            } else {
                finalGamma[i] = 1;
                finalEpsilon[i] = 0;
            }
        }

        int gammaDec = 0;
        int epsilonDec = 0;

        for (int i = 0; i < finalEpsilon.length; i++) {
            gammaDec += (int) Math.pow(2.0, (float) i) * finalGamma[finalGamma.length - i - 1];
            epsilonDec += (int) Math.pow(2.0, (float) i) * finalEpsilon[finalEpsilon.length - i - 1];
        }
        System.out.println("gammaDec: " + gammaDec);
        System.out.println("epsilonDec: " + epsilonDec);
        System.out.println("FINAL: " + gammaDec * epsilonDec);

    }
}