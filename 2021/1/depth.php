<?php 
    $rawdata = file_get_contents("data.txt");
    $data = explode(PHP_EOL, $rawdata);
    $count = 0;
    //print($data[0]);
    //print($data[count($data)-1]);
    for($i=1;$i<count($data);$i++){
        if($data[$i]>$data[$i-1]){
            $count++;
        }
    }
    print($count);
    //var_dump($data);
?>