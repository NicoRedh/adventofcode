import java.io.BufferedReader;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.List;

public class Bingo {

    public static List<int[][]> readBoards(String filename) {
        List<int[][]> data = new ArrayList<int[][]>();
        
        try {
            FileReader reader = new FileReader(filename);
            try (BufferedReader buffReader = new BufferedReader(reader)) {
                String[][] input = new String[5][5];
                buffReader.readLine();
                while ((buffReader.readLine()) != null) {
                    int[][] intInput = new int[5][5];
                    for (int i = 0; i < 5; i++) {
                        input[i] = buffReader.readLine().split(" ");
                    }
                    for (int i = 0; i < 5; i++) {
                        for (int j = 0; j < 5; j++) {
                            intInput[i][j] = Integer.parseInt(input[i][j]);
                        }
                    }
                    data.add(intInput);
                }

            }
        } catch (Exception e) {
            System.out.println("ERROR: file not found \n" + e);
        }
        return data;
    }

    public static List<Integer> readNumbers(String filename) {
        List<Integer> data = new ArrayList<Integer>();
        try {
            FileReader reader = new FileReader(filename);
            try (BufferedReader buffReader = new BufferedReader(reader)) {
                String[] nums = (buffReader.readLine().split(","));
                for (String num : nums) {
                    data.add(Integer.parseInt(num));
                }
            }
        } catch (Exception e) {
            System.out.println("ERROR: file not found \n" + e);
        }
        return data;
    }

    public static void main(String[] args) {
        List<int[][]> boardInts = readBoards("input.txt");
        List<Board> boards = new ArrayList<Board>();
        for(int[][] bored : boardInts){
            boards.add(new Board(bored));
        }
        List<Integer> numbers = readNumbers("input.txt");
        end: for (Integer nums : numbers) {
            for (Board board : boards) {
                if (board.check(nums)) {
                    System.out.println(board.winner(nums));
                    break end;
                }
            }
        }
    }
}
