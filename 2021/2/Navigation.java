import java.io.BufferedReader;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.List;

public class Navigation{
    public static List<String[]> readFile(String filename){
        List<String[]> data = new ArrayList<String[]>();
        try {
                FileReader reader = new FileReader(filename);
                try (BufferedReader buffReader = new BufferedReader(reader)) {
                    String line;
                    String[] input = new String[2];
                    while((line=buffReader.readLine())!=null){
                        input = line.split(" ");
                        data.add(input);
                       // System.out.println(lineint);
                    }
                }
            } catch (Exception e) {
               System.out.println("ERROR: file not found \n" + e );
            }
            return data;
        }
    
    public static void main(String[] args) {
        List<String[]> data = readFile(args[0]);
        int y =0;
        int x = 0;

        for(int i = 0; i<data.size(); i++){
            switch (data.get(i)[0]){
                case "forward":
                    x += Integer.parseInt(data.get(i)[1]);
                    break;
                case "down": 
                    y += Integer.parseInt(data.get(i)[1]);
                    break;
                case "up": 
                    y -= Integer.parseInt(data.get(i)[1]);
                    break;
            }
        }
        
        System.out.println(x*y);
    }
}