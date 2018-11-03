boolean isPrime(int n){
  if(n<=1){
    return false;
  }
  for(int i = 2; i < floor(sqrt(n))+1;i++){
    if(n % i == 0){
      return false;
    }
  }
  return true;
}
boolean isPalindrome(int n){
  String s = Integer.toString(n);
  return s.equals(reverse(s));
}
String reverse(String input){
    char[] in = input.toCharArray();
    int begin=0;
    int end=in.length-1;
    char temp;
    while(end>begin){
        temp = in[begin];
        in[begin]=in[end];
        in[end] = temp;
        end--;
        begin++;
    }
    return new String(in);
}
void setup(){
  int count = 0;
  for(int n = 10000000;n<100000000;n++){
    if(!isPrime(n) && isPalindrome(n)){
      count++;
    }
  }
  println("done");
  println(count);
  print(count);
}