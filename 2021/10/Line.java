import java.util.*;

public class Line {
    private List<Integer> line;
    
    public Line(List<Integer> line){
        this.line = line;
    }
    
    public int checkCorrupt(){
        int wrongParens = 0;
        Deque<Integer> parens = new ArrayDeque<Integer>();

        for(int l : line){
            if(l>0){
                parens.add(l);
            }else if(l<0){
                if(parens.getLast() != -l){
                    return -l;
                }else{
                    if(!parens.isEmpty())
                    parens.removeLast();
                }
            } 
        }
        
        return wrongParens;
    }
    public long checkIncomplete(){
        //if(this.checkCorrupt()!=0) return -1;
        long score = 0;
        Deque<Integer> parens = new ArrayDeque<Integer>();
        //System.out.println(parens.isEmpty());
        for(int l : line){
            if(l>0){
                parens.add(l);
               // System.out.println("l: " + l);
            }else if(l<0){
                if(parens.getLast() == -l)
                parens.removeLast();
               // System.out.println("l: " + l);
            }
        }
        //System.out.println("parens.length: " + parens.size());
        while(!parens.isEmpty()){
            switch(parens.getLast()){
                case(3):
                    score = (score * 5) +1;
                    parens.removeLast();
                    break;
                case(57):
                    score = (score*5) +2;
                    parens.removeLast();
                    break;
                case(1197):
                    score = (score *5) +3;
                    parens.removeLast();
                    break;
                case(25137):
                    score = (score*5)+4;
                    parens.removeLast();
                    break;
                default:
                    break;

            }
        }

        return score;
    }
}
