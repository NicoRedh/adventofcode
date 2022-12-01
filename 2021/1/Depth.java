import java.io.BufferedReader;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.List;

public class Depth{
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
        //System.out.println(list.get(0));
        for(int i = 1; i<list.size();i++){
            if(list.get(i)>list.get(i-1)){
                count++;
            }
        }
        System.out.println(count);
    }
}