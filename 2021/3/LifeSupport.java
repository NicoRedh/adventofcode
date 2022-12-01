import java.io.BufferedReader;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.List;

public class LifeSupport {
    public static List<String[]> readFile(String filename) {
        List<String[]> data = new ArrayList<String[]>();
        try {
            FileReader reader = new FileReader(filename);
            try (BufferedReader buffReader = new BufferedReader(reader)) {

                String line = buffReader.readLine();
                String[] input = new String[line.length()];

                do {
                    input = (line.split(""));
                    data.add(input);
                    // System.out.println(input.length);
                } while ((line = buffReader.readLine()) != null);
            }
        } catch (Exception e) {
            System.out.println("ERROR: file not found \n" + e);
        }
        return data;
    }

    public static int zerocounts(List<String[]> data, int index) {
        int count = 0;
        for (int i = 0; i < data.size(); i++) {
            if (Integer.parseInt(data.get(i)[index]) == 0)
                count++;
        }
        return count;
    }

    public static void main(String[] args) {
        List<String[]> data = readFile(args[0]);
        List<String[]> OTwo = readFile(args[0]);
        List<String[]> COTwo = readFile(args[0]);
        System.out.println("binNumber Length: " + data.get(0).length);
        // int[] zerocounts = new int[data.get(0).length];
        // int[] onecounts = new int[data.get(0).length];

        for (int i = 0; i < data.get(0).length; i++) {
            int OTwoCount = zerocounts(OTwo, i);
            int COTwoCount = zerocounts(COTwo, i);
            List<Integer> OTwoRemove = new ArrayList<Integer>();
            List<Integer> COTwoRemove = new ArrayList<Integer>();
            if (OTwoCount > OTwo.size() - OTwoCount) {
                // O2
                    for (int k = 0; k < OTwo.size(); k++) {
                        if (Integer.parseInt(OTwo.get(k)[i]) == 1){
                            OTwoRemove.add(k);
                        }
                    }
            } else {              
                    for (int k = 0; k < OTwo.size(); k++) {
                        if (Integer.parseInt(OTwo.get(k)[i]) == 0){
                            OTwoRemove.add(k);
                        }
                    }
                }        
            if (COTwoCount > COTwo.size() - COTwoCount) {               
                    for (int k = 0; k < COTwo.size(); k++) {
                        if (Integer.parseInt(COTwo.get(k)[i]) == 0){
                            COTwoRemove.add(k);
                        }
                    }                
            } else {                
                    for (int k = 0; k < COTwo.size(); k++) {
                        if (Integer.parseInt(COTwo.get(k)[i]) == 1){
                            COTwoRemove.add(k);
                        }
                    }               
            }
            for (int l = OTwoRemove.size()-1; l>=0; l--){
                if(OTwo.size()>1){
                    int OTwoIndex = OTwoRemove.get(l);
                    OTwo.remove(OTwoIndex);
                }
            }
            for (int l = COTwoRemove.size()-1; l>=0; l--){
                if(COTwo.size()>1){
                    int COTwoIndex = COTwoRemove.get(l);
                    COTwo.remove(COTwoIndex);
                }
            }

        }
        // System.out.println("zerocounts: " + zerocounts[0]);

        
        int COTwoDec = 0;
        int OTwoDec = 0;

        for (int i = 0; i < data.get(0).length; i++) {
            OTwoDec += (int) Math.pow(2.0, (float) i) * Integer.parseInt(OTwo.get(0)[data.get(0).length - i - 1]);
            COTwoDec += (int) Math.pow(2.0, (float) i) * Integer.parseInt(COTwo.get(0)[data.get(0).length - i - 1]);
        }
        System.out.println("OTwoDec: " + OTwoDec);
        System.out.println("COTwoDec: " + COTwoDec);
        System.out.println("FINAL: " + OTwoDec * COTwoDec);

    }
}
