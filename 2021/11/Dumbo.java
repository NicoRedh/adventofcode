import java.io.*;

public class Dumbo {
    public static int[][] readFile(String filename) {
        int[][] data = new int[10][10];
        try (FileReader freader = new FileReader(filename);
                BufferedReader buffReader = new BufferedReader(freader)) {
            for (int j = 0; j < 10; j++) {
                String line = buffReader.readLine();
                String[] lineSplit = line.split("");
                for (int i = 0; i < lineSplit.length; i++) {
                    data[j][i] = Integer.parseInt(lineSplit[i]);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return data;
    }

    public static void main(String[] args) {
        int count[] = {0};
        int[][] data = readFile("input.txt");
        // for (int k = 0; k < 100; k++) {
        //     boolean[][] lighted = new boolean[10][10];
        //     for (int i = 0; i < 10; i++) {
        //         for (int j = 0; j < 10; j++) {
        //             data[i][j]++;
        //             checkNine(i,j,data,lighted,count);
        //         }
        //     }
        //     for (int i = 0; i < 10; i++) {
        //         for (int j = 0; j < 10; j++) {
        //            if(lighted[i][j]){
        //                data[i][j] = 0;
        //            }
        //         }
        //     }
            
        // }
        // System.out.println(count[0]);

        //part 2
        boolean sync = false;
        int count2 = 0;
        while (!sync) {
            boolean zero = true;
            for (int i = 0; i < 10; i++) {
                for (int j = 0; j < 10; j++) {
                    if(data[i][j] != 0) zero = false;
                }
            }
            if(zero){
                sync = true;
                break;
            }
            count2++;
            boolean[][] lighted = new boolean[10][10];
            for (int i = 0; i < 10; i++) {
                for (int j = 0; j < 10; j++) {
                    data[i][j]++;
                    checkNine(i,j,data,lighted,count);
                }
            }
            for (int i = 0; i < 10; i++) {
                for (int j = 0; j < 10; j++) {
                   if(lighted[i][j]){
                       data[i][j] = 0;
                   }
                }
            }
            
        }
        System.out.println(count2);
    }

    public static void checkNine(int i, int j, int[][] data, boolean[][] lighted, int[] count) {
        if(data[i][j] > 9 && !lighted[i][j]){
            count[0]++;
            lighted[i][j] = true;
            if(i!=0){
                data[i-1][j]++;
                checkNine(i-1, j, data, lighted, count);
            }
            if(i!=9){
                data[i+1][j]++;
                checkNine(i+1, j, data, lighted, count);
            }
            if(j!=0){
                data[i][j-1]++;
                checkNine(i, j-1, data, lighted, count);
            }
            if(j!=9){
                data[i][j+1]++;
                checkNine(i, j+1, data, lighted, count);
            }
            if(i!=0 && j!=0){
                data[i-1][j-1]++;
                checkNine(i-1, j-1, data, lighted, count);
            }
            if(i!=9 && j!= 0){
                data[i+1][j-1]++;
                checkNine(i+1, j-1, data, lighted, count);
            }
            if(i != 9 && j != 9){
                data[i+1][j+1]++;
                checkNine(i+1, j+1, data, lighted, count);
            }
            if(i!=0 && j!=9){
                data[i-1][j+1]++;
                checkNine(i-1, j+1, data, lighted, count);
            }
        }
    }
}