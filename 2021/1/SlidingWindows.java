import java.io.BufferedReader;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.List;
public class SlidingWindows {
    public static List<Integer> readFile(String filename){
        List<Integer> data = new ArrayList<Integer>();
        try {
            FileReader reader = new FileReader(filename);
            try (BufferedReader buffReader = new BufferedReader(reader)) {
                String line;
                int lineint;
                while((line=buffReader.readLine())!=null){
                    lineint = Integer.parseInt(line);
                    data.add(lineint);
                   // System.out.println(lineint);
                }
            }
        } catch (Exception e) {
           System.out.println("ERROR: file not found \n" + e );
        }
        return data;
    }

    public static void main(String[] args) {
        List<Integer> list = readFile(args[0]);
        int count = 0;
        int firstThree = 0;
        int secondThree = 0;
        //System.out.println(list.get(0));
        for(int i = 3; i<list.size();i++){
            firstThree = list.get(i-1) + list.get(i-2) + list.get(i-3);
            secondThree = list.get(i) + list.get(i-1) + list.get(i- 2);
            if(secondThree>firstThree){
                count++;
            }
        }
        System.out.println(count);
    }
}
