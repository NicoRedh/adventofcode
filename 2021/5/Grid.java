import java.util.List;

public class Grid {
    private int[][] grid;

    public Grid(List<int[]> input) {
        int ymax = 0; // find the maximum value to determine gridsize.
        int xmax = 0;
        for (int i = 0; i < input.size(); i++) {
            if (input.get(i)[0] > xmax)
                xmax = input.get(i)[0]; // x1
            if (input.get(i)[1] > ymax)
                ymax = input.get(i)[1]; // y1
            if (input.get(i)[2] > xmax)
                xmax = input.get(i)[2]; // x2
            if (input.get(i)[3] > ymax)
                ymax = input.get(i)[3]; // y2
        }
        this.grid = new int[ymax + 1][xmax + 1];
    }

    public void drawLines(List<int[]> input) {
        for (int[] in : input) {
            // part1 check if horizontal or vertical
            if (in[0] == in[2]) {
                // 'draw Line'
                int min = Math.min(in[1], in[3]);
                int max = Math.max(in[1], in[3]);
                for (int i = min; i < max + 1; i++) {
                    // increment the Gridpoints from smaller value
                    grid[i][in[0]]++;
                }
            }
            if (in[1] == in[3]) {
                // 'draw Line'
                int min = Math.min(in[0], in[2]);
                int max = Math.max(in[0], in[2]);
                for (int i = min; i < max + 1; i++) {
                    // increment the Gridpoints from smaller value
                    grid[in[1]][i]++;
                }
            }
        }
    }

    public void drawDiagonals(List<int[]> input) {
        int countif = 0;
        int countreal = 0;
        for (int[] in : input) {
            if (Math.abs(in[0] - in[2]) == Math.abs(in[1] - in[3])) {
                // for(int i = 0; i<4; i++){
                // System.out.print(in[i] + " ");
                // }System.out.print("\n");
                countif += Math.abs(in[0] - in[2]) + 1;
                if (in[0] < in[2] && in[1] < in[3]) { // go lower right
                    for (int i = 0; i < Math.abs(in[0] - in[2]) + 1; i++) {
                        grid[in[1] + i][in[0] + i]++;
                        countreal++;
                    }
                } else if (in[0] > in[2] && in[1] > in[3]) { // go lower left
                    for (int i = 0; i < Math.abs(in[0] - in[2]) + 1; i++) {
                        grid[in[1] - i][in[0] - i]++;
                        countreal++;
                    }
                } else if (in[0] < in[2] && in[1] > in[3]) { // go upper right
                    for (int i = 0; i < Math.abs(in[0] - in[2]) + 1; i++) {
                        grid[in[1] - i][in[0] + i]++;
                        countreal++;
                    }
                } else if (in[0] > in[2] && in[1] < in[3]) { // go upper left
                    for (int i = 0; i < Math.abs(in[0] - in[2]) + 1; i++) {
                        grid[in[1] + i][in[0] - i]++;
                        countreal++;
                    }
                }
            }
        }
        System.out.println("countif: " + countif);
        System.out.println("countreal: " + countreal);
    }

    public int checkOverlaps() {
        int count = 0;
        for (int i = 0; i < grid.length; i++) {
            for (int j = 0; j < grid[0].length; j++) {
                if (grid[i][j] >= 2)
                    count++;
            }
        }
        return count;
    }

    public void printGrid() {
        for (int i = 0; i < grid.length; i++) {
            for (int j = 0; j < grid[0].length; j++) {
                System.out.print(" " + grid[i][j]);
            }
            System.out.println();
        }
    }
}