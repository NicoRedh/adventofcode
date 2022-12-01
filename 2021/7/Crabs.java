import java.io.BufferedReader;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.List;

public class Crabs{
    public static List<Integer> readInput(String filename){
        List<Integer> data = new ArrayList<Integer>();
        try{
            FileReader freader = new FileReader(filename);
            try(BufferedReader buffReader = new BufferedReader(freader);){
                String[] input = buffReader.readLine().split(",");
                for(String in : input){
                    data.add(Integer.parseInt(in));
                }
            }catch(Exception f){
                System.out.println(f);
            }
            
        }catch(Exception e){
            System.out.println(e);
        }

        return data;
    }
    public static void main(String[] args) {
        List<Integer> data = readInput("input.txt");
        int min = 0;
        int max = 0;
        for(int da :data){
            if(da<min) min = da;
            if(da>max) max = da;
        }
        int minFuel = Integer.MAX_VALUE;
        for(int i = min; i<=max; i++){
            int fuel = 0;
            for(int da : data){
                fuel+=Math.abs(da-i);
            }
            if(minFuel>fuel) minFuel = fuel;
        }
        System.out.println(minFuel);
        //part2
        minFuel = Integer.MAX_VALUE;
        for(int i = min; i<=max; i++){
            int fuel = 0;
            for(int da : data){
                int n = Math.abs(da-i);
                //System.out.println(n);
                fuel += ((n*n) +n)/2;
            }
            if(minFuel>fuel) minFuel = fuel;
        }
        System.out.println(minFuel);
    }
    
}