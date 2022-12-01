import java.io.*;
import java.util.*;

import javax.sound.midi.Synthesizer;

public class Smoke {
    public static List<int[]> readFile(String filename) {
        List<int[]> data = new ArrayList<int[]>();
        try (FileReader freader = new FileReader(filename)) {
            try (BufferedReader buffReader = new BufferedReader(freader)) {
                String line = "";
                while ((line = buffReader.readLine()) != null) {
                    String[] stringInput = line.split("");
                    int[] intInput = new int[stringInput.length];
                    for (int i = 0; i < intInput.length; i++) {
                        intInput[i] = Integer.parseInt(stringInput[i]);
                    }

                    data.add(intInput);
                }

            } catch (Exception e) {
                e.printStackTrace();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }

        return data;
    }

    public static void main(String[] args) {
        List<int[]> data = readFile("input.txt");
        int count = 0;

        for (int i = 0; i < data.size(); i++) {
            for (int j = 0; j < data.get(i).length; j++) {
                boolean larger = true;
                int currentNum = data.get(i)[j];
                if (j != 0) {
                    if (data.get(i)[j - 1] <= currentNum) {
                        larger = false;
                        continue;
                    }
                }
                if (j != data.get(i).length - 1) {
                    if (data.get(i)[j + 1] <= currentNum) {
                        larger = false;
                        continue;
                    }
                }
                if (i != 0) {
                    if (data.get(i - 1)[j] <= currentNum) {
                        larger = false;
                        continue;
                    }
                }
                if (i != data.size() - 1) {
                    if (data.get(i + 1)[j] <= currentNum) {
                        larger = false;
                        continue;
                    }
                }
                if (larger == true)
                    count += currentNum + 1;
            }
        }
        System.out.println(count);
        // part 2:
        boolean[][] nine = new boolean[data.size()][data.get(0).length];
        boolean[][] checked = new boolean[data.size()][data.get(0).length];
        boolean[][] hChecked = new boolean[data.size()][data.get(0).length];
        boolean[][] vChecked = new boolean[data.size()][data.get(0).length];
        int[][] colors = new int[data.size()][data.get(0).length];
        int basinCount = 1;
        for (int i = 0; i < data.size(); i++) {
            for (int j = 0; j < data.get(i).length; j++) {
                if (data.get(i)[j] == 9) {
                    nine[i][j] = true;
                    checked[i][j] = true;
                    hChecked[i][j] = true;
                    vChecked[i][j] = true;
                }

            }
        }
        for (int i = 0; i < nine.length; i++) {
            for (int j = 0; j < nine[i].length; j++) {
                if (hChecked[i][j])
                    continue;
                int[] leftj = { checked[0].length - 1 };
                int[] lefti = { 0 };
                checkRow(i, j, basinCount, hChecked, colors, leftj, lefti);
                checkColumn(lefti[0], leftj[0], basinCount, vChecked, colors);
                // checkColumn()
                basinCount++;
                // for (int k = 0; k < 5; k++) {
                // for (int l = 0; l < 10; l++) {
                // System.out.print(" " + colors[k][l]);
                // }
                // System.out.println();
                // }
                // System.out.println();
            }
        }
        int[] finalCount = new int[basinCount];
        for (int i = 0; i < colors.length; i++) {
            for (int j = 0; j < colors[i].length; j++) {
                for (int k = 1; k < finalCount.length; k++) {
                    if (colors[i][j] == k)
                        finalCount[k]++;
                }
            }
        }
        for (int fin : finalCount) {
            System.out.println(fin);
        }
        // List finalList = Arrays.asList(finalCount);
        int[] finalThree = new int[3];
        for (int i = 0; i < finalThree.length; i++) {
            finalThree[i] = Arrays.stream(finalCount).max().getAsInt();
            for (int g = 0; g < finalCount.length; g++) {
                if (finalCount[g] == Arrays.stream(finalCount).max().getAsInt()) {
                    finalCount[g] = 0;
                    break;
                }
            }
        }
        System.out.println(finalThree[0] * finalThree[1] * finalThree[2]);
        
        try{StringBuilder builder = new StringBuilder();
        for (int i = 0; i < colors.length; i++)// for each row
        {
            for (int j = 0; j < colors.length; j++)// for each column
            {
                builder.append(colors[i][j] + "");// append to the output string
                if (j < colors.length - 1)// if this is not the last row element
                    builder.append(",");// then add comma (if you don't like commas you can use spaces)
            }
            builder.append("\n");// append new line at the end of the row
        }
        BufferedWriter writer = new BufferedWriter(new FileWriter("out.txt"));
        writer.write(builder.toString());// save the string representation of the board
        writer.close();}
        catch(Exception e){

        }
    }

    public static void checkRow(int i, int j, int basinCount, boolean[][] checked, int[][] colors, int[] leftj,
            int[] lefti) {
        List<Integer> down = new ArrayList<Integer>();
        j--;
        do { // einmal die reihe durch wenn nicht schon am ende des Felds
            if (j != checked[i].length - 1) {
                j++;
            }
            if (colors[i][j] == 0)
                colors[i][j] = basinCount;
            checked[i][j] = true;
            if (i != checked.length - 1) {
                if (!checked[i + 1][j])
                    down.add(j); // hier gibt es einen Eintrag unter dem Element
            }
            if (j == checked[i].length - 1)
                break;
        } while (!checked[i][j + 1]);
        if (down.isEmpty())
            return;
        if (i == checked.length - 1)
            return;
        int newj = checked[i].length - 1;
        for (int d : down) {
            if (d < newj)
                newj = d;
        }

        if (j != 0) {
            if (newj != 0) {
                while (!checked[i + 1][newj - 1] && newj != 0) {
                    newj--;
                    if (newj == 0)
                        break;
                }
            }
        }
        if (newj < leftj[0]) {
            leftj[0] = newj;
            lefti[0] = i + 1;
        }
        checkRow(i + 1, newj, basinCount, checked, colors, leftj, lefti);

    }

    public static void checkColumn(int i, int j, int basinCount, boolean[][] checked, int[][] colors) {
        // System.out.println("j: " + j);
        List<Integer> down = new ArrayList<Integer>();
        i++;
        do { // einmal die spalte durch wenn nicht schon am ende des Felds
            if (i != 0) {
                i--;
            }
            if (colors[i][j] == 0)
                colors[i][j] = basinCount;
            checked[i][j] = true;
            if (j != checked[i].length - 1) {
                if (!checked[i][j + 1])
                    down.add(i); // hier gibt es einen Eintrag rechts vom Element
            }
            if (i == 0)
                break;
        } while (!checked[i - 1][j]);
        if (down.isEmpty())
            return;
        if (j == checked[i].length - 1)
            return;
        int newi = checked.length - 1;
        for (int d : down) {
            if (d < newi)
                newi = d;
        }

        if (i != checked.length - 1) {
            if (newi != checked.length - 1) {
                while (!checked[newi + 1][j + 1] && newi != checked.length - 1) {
                    newi++;
                    if (newi == checked.length - 1)
                        break;
                }
            }
        }
        checkColumn(newi, j + 1, basinCount, checked, colors);
    }
}