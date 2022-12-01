<?php 
    $rawdata = file_get_contents("data.txt");
    $data = explode(PHP_EOL, $rawdata);
    $count = 0;
    //print($data[0]);
    //print($data[count($data)-1]);
    for($i=3;$i<count($data);$i++){
        $firstThree = ($data[$i-1] + $data[$i-2] + $data[$i-3]);
        $secondThree = ($data[$i] + $data[$i-1] + $data[$i-2]);
        print("first Three: ".$firstThree.PHP_EOL);
        if($secondThree>$firstThree){
            $count++;
        }
    }
    print($count);
    //var_dump($data);
?>