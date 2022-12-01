public class Display {
    private String[] digits = new String[4];
    private String[] notes = new String[10];

    public Display(String[] digits, String[] notes){
        this.digits = digits;
        this.notes = notes;
    }

    public int countDefNums(){
        int count = 0;
        for(String digit : digits){
            if(digit.length() == 2 || digit.length() == 3 || digit.length() == 4 || digit.length() == 7) count++;
        }
        return count;
    }

    public int decipherDigits(){
        int[] finalNums = new int[4];
        String bd[] = null;
        String cf[] = null;
        //String[] numbers = new String[10];
        for(String note : notes) if(note.length() ==2) cf=note.split("");
        for(String note : notes) if(note.length() == 4){

            bd = note.replaceAll(cf[0], "").replaceAll(cf[1], "").split("");
        } 
        //for(String note : notes){
        //    if(note.length() == 6 && (!note.contains(bd[0]) || !note.contains(bd[1]))) numbers[0] = note;
        //    if(note.length() == 2) numbers[1] = note;
        //    if(note.length() == 5 && (!note.contains(bd[0]) || !note.contains(bd[1])) &&(!note.contains(cf[0]) || !note.contains(cf[1]))) numbers[2] = note;
        //    if(note.length() == 5 && note.contains(cf[0]) && note.contains(cf[1])) numbers[3] = note;
        //    if(note.length() == 4) numbers[4] = note;
        //    if(note.length() == 5 && (!note.contains(cf[0]) || !note.contains(cf[1])) && note.contains(bd[0]) && note.contains(bd[1])) numbers[5] = note;
        //    if(note.length() == 6 && (!note.contains(cf[0]) || !note.contains(cf[1]))) numbers[6] = note;
        //    if(note.length() == 3) numbers[7] = note;
        //    if(note.length() == 7) numbers[8] = note;
        //    if(note.length() == 6 && note.contains(bd[0]) && note.contains(bd[1]) && note.contains(cf[0]) && note.contains(cf[1])) numbers[9] = note;
        //}
        
        for(int i = 0; i<4; i++){
            
            switch (digits[i].length()){
                case 2:
                    finalNums[i] = 1;
                    break;
                case 3:
                    finalNums[i] = 7;
                    break;
                case 4:
                    finalNums[i] = 4;
                    break;
                case 5:
                    if((!digits[i].contains(bd[0]) || !digits[i].contains(bd[1])) &&(!digits[i].contains(cf[0]) || !digits[i].contains(cf[1]))) finalNums[i] = 2;
                    if(digits[i].contains(cf[0]) && digits[i].contains(cf[1])) finalNums[i] = 3;
                    if((!digits[i].contains(cf[0]) || !digits[i].contains(cf[1])) && digits[i].contains(bd[0]) && digits[i].contains(bd[1])) finalNums[i] = 5;
                    break;
                case 6:
                    if(digits[i].length() == 6 && (!digits[i].contains(bd[0]) || !digits[i].contains(bd[1]))) finalNums[i] = 0;
                    if(digits[i].length() == 6 && (!digits[i].contains(cf[0]) || !digits[i].contains(cf[1]))) finalNums[i] = 6;
                    if(digits[i].length() == 6 && digits[i].contains(bd[0]) && digits[i].contains(bd[1]) && digits[i].contains(cf[0]) && digits[i].contains(cf[1])) finalNums[i] = 9;
                    break;
                case 7:
                    finalNums[i] = 8;
            }
        }
        String finalString = "";
        for(int num : finalNums){
            finalString += num;
        }
        int fin = Integer.parseInt(finalString);


        return fin;
    }

}
