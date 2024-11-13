#include <SD.h>
#include <SPI.h>

File myFile;

int pinCS = 10;
int value = 0;
float voltage;
unsigned long time;

void setup(){
  Serial.begin(9600);
  pinMode(pinCS, OUTPUT);
  //analogReference(INTERNAL);
  
  // SD Card Initialization
  if (SD.begin())
  {
    Serial.println("SD card is ready to use.");
  } else
  {
    Serial.println("SD card initialization failed");
    return;
  }
}

void loop(){
  // Obtain the voltage reading
  value = analogRead(A0);
  voltage = value * 5.0/1023 * 1000;
  
  // Obtain the time from initialization in the format HH:MM:SS
  time = (millis()/1000);
  int hr = time/3600;                                                        
  int mins = (time-hr*3600)/60;                                              
  int sec = time-hr*3600-mins*60;                                            
  String hrMinSec = (String(hr) + ":" + String(mins) + ":" + String(sec));   
  
  // View the readings on the serial monitor
  Serial.print("Time=");
  Serial.print(hrMinSec);
  Serial.print(", ");
  Serial.print("Voltage=");
  Serial.println(voltage);
  
  // Write the output to the SD card
  myFile = SD.open("example.txt", FILE_WRITE);
  if (myFile) {
    myFile.print("Time=");
    myFile.print(hrMinSec);
    myFile.print(", ");
    myFile.print("Voltage=");
    myFile.println(voltage);
    myFile.close(); // close the file
  }

  // if the file didn't open, print an error:
  else {
    Serial.println("error opening file");
  }
  
  // Choose the time interval for each reading. Note: 1000 = 1 second
  delay(1000);
}

// GENERAL NOTES ABOUT THE ARDUINO CODE:
// 1) The file name CANNOT exceed 8 characters (including special characters)
// 2) You must name a new file after each upload (the files are NEVER overwritten or appended to)
// 3) If the Arduino accidentally powers off, you will need to re-upload the code. Use a new file name, or the new data will be lost
// 4) An EBL rechargeable battery will only last 2 hours and 50 minutes. Need to determine a long-term cordless power solution
