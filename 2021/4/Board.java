public class Board {
    private boolean[][] marked;
    private int[][] nums;
    private boolean won = false;

    public Board(){
        nums = new int[5][5];
        marked = new boolean[5][5];
    }

    public Board(int[][] num){
        this.nums = num;
        this.marked = new boolean[5][5];
    }
    public void setNums(int[][] nums) {
        this.nums = nums;
    }
    public boolean check(int num){
        for(int i = 0; i<5; i++){
            for(int j = 0; j<5; j++){
                if(nums[i][j] == num) marked[i][j] = true;
            }
        }
        for(int i = 0; i<5; i++){
            if(marked[i][0] == true
               && marked[i][1] == true
               && marked[i][2] == true
               && marked[i][3] == true
               && marked[i][4] == true) won = true;
        }
        for(int j = 0; j<5; j++){
            if(marked[0][j] == true
               && marked[1][j] == true
               && marked[2][j] == true
               && marked[3][j] == true
               && marked[4][j] == true) won = true;
        }
        return won;
    }
    public int winner(int num){
        int sum = 0 ;
        for(int i = 0; i<5; i++){
            for(int j = 0; j<5; j++){
                if(marked[i][j] == false) sum += nums[i][j];
            }
        }
        return sum*num;
    }
}
