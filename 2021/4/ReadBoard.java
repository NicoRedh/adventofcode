import java.io.BufferedReader;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.List;

public class ReadBoard {
    private List<Board> boards;
    ReadBoard(String file){
        this.boards = readBoards(file);
    }
    public List<Board> getBoards() {
        return boards;
    }
    public List<Board> readBoards(String filename) {
        List<Board> data = new ArrayList<Board>();
        
        try {
            FileReader reader = new FileReader(filename);
            try (BufferedReader buffReader = new BufferedReader(reader)) {
                String[][] input = new String[5][5];
                int[][] intInput = new int[5][5];
                buffReader.readLine();
                Board boardnew;
                while ((buffReader.readLine()) != null) {
                    for (int i = 0; i < 5; i++) {
                        input[i] = buffReader.readLine().split(" ");
                    }
                    for (int i = 0; i < 5; i++) {
                        for (int j = 0; j < 5; j++) {
                            intInput[i][j] = Integer.parseInt(input[i][j]);
                        }
                    }
                    data = new ArrayList<Board>();
                    boardnew = new Board(intInput);
                    data.add(boardnew);
                    }
                return data;
            }
        } catch (Exception e) {
            System.out.println("ERROR: file not found \n" + e);
            return data;
        }
        
    }

}
