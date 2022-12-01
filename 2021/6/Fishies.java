import java.util.List;

public class Fishies {
    private List<Integer> fishies;
    private long[] fishcount = new long[9];

    public List<Integer> getFishies() {
        return fishies;
    }

    public Fishies(List<Integer> fish) {
        for (int s : fish) {
            switch (s) {
                case 0:
                    fishcount[0]++;
                    break;
                case 1:
                    fishcount[1]++;
                    break;
                case 2:
                    fishcount[2]++;
                    break;
                case 3:
                    fishcount[3]++;
                    break;
                case 4:
                    fishcount[4]++;
                    break;
                case 5:
                    fishcount[5]++;
                    break;
                case 6:
                    fishcount[6]++;
                    break;
                case 7:
                    fishcount[7]++;
                    break;
                case 8:
                    fishcount[8]++;
                    break;
            }
        }
    }

    public void simDay() {
        long newborns = fishcount[0];
        fishcount[0] = fishcount[1];
        fishcount[1] = fishcount[2];
        fishcount[2] = fishcount[3];
        fishcount[3] = fishcount[4];
        fishcount[4] = fishcount[5];
        fishcount[5] = fishcount[6];
        fishcount[6] = fishcount[7] + newborns;
        fishcount[7] = fishcount[8];
        fishcount[8] = newborns;

    }
    public long countFish(){
        long count = 0;
        for(int i = 0; i<9; i++){
            count += fishcount[i];
        }
        return count;
    }
}
