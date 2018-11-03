boolean[][] arrClone(boolean[][] arr){
  boolean[][] ans = new boolean[arr.length][arr[0].length]; 
  for(int r = 0;r<arr.length;r++){
    for(int c = 0;c<arr[0].length;c++){
      ans[r][c] = arr[r][c];
    }
  }
  return ans;
}
int neighbors(boolean[][] arr,int r, int c){//# of live neighbors
  int ans = 0;
  for(int y = -1;y<=1;y++){
    for(int x = -1;x<=1;x++){
      if(!(x==0&&y==0) && inBounds(arr,r+y,c+x) && arr[r][c])//never throws index exc because of lazy evaluation
        ans++;
    }
  }
}

boolean inBounds(boolean[][] arr, int r, int c){
  return r>=0 && c>=0 && r<arr.length && c<arr[0].length;
}
boolean[][] nextBoard(boolean[][]arr){
  boolean[][] ans = new boolean[arr.length][arr[0].length];
  for(int r = 0;r<arr.length;r++){
    for(int c = 0;c<arr[0].length;c++){
      int n = neighbors(arr,r,c);
      if(arr[r][c]){
        if(n==2||n==3)
          ans[r][c] = true;
      }
      else if(n==3)
        ans[r][c] = true;
      else
        ans[r][c] = false;
    }
  }
}