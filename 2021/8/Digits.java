import java.io.BufferedReader;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.List;

public class Digits{
    public static List<Display> readFile(String filename){
        List<Display> data = new ArrayList<Display>();

        try(
        FileReader freader = new FileReader(filename);
        BufferedReader buffReader = new BufferedReader(freader);
        ){
            String line;
            while((line = buffReader.readLine())!=null){
                String[] splitter = line.split(" ");
                String[] notes = new String[10];
                String[] digits = new String[4];
                
                for(int i = 0; i<10; i++){
                    notes[i] = splitter[i];
                }
                for(int i = 11; i<15; i++){
                    digits[i-11] = splitter[i];
                }
                data.add(new Display(digits, notes));
            }
        }catch(Exception e){
            e.printStackTrace();
        }
        return data;
    }
    public static void main(String[] args) {
        List<Display> data = readFile("input.txt");
        int count = 0;
        for(Display d : data){
            count += d.countDefNums();
        }
        System.out.println(count);

        int sum = 0;
        for(Display d : data){
            sum += d.decipherDigits();
        }

        System.out.println(sum);
    }
}