class: inverse, center, middle

# Lightweight Monadic Region

## CindyLinz

2018.4.5

---
layout: true

## 稀缺資源使用 pattern

<svg width=800 height=110>
  <defs>
    <g id=arrow>
      <path d="M0,-4 l75,0 l0,-5 l25,9 l-25,9 l0,-5 l-75,0 z" fill=#000 />
    </g>
  </defs>
  <g transform='translate(0 0)'>
    <rect x=10 y=10 width=100 height=62 stroke=#000 stroke-width=3 fill=transparent />
    <text x=60 y=48 text-anchor=middle alignment-baseline=central>allocate</text>
    <use xlink:href=#arrow x=120 y=43 />
    <rect x=230 y=10 width=100 height=62 stroke=#000 stroke-width=3 fill=transparent />
    <text x=280 y=48 text-anchor=middle alignment-baseline=central>use</text>
    <use xlink:href=#arrow x=340 y=43 />
    <rect x=450 y=10 width=100 height=62 stroke=#000 stroke-width=3 fill=transparent />
    <text x=500 y=48 text-anchor=middle alignment-baseline=central>free</text>
  </g>
</svg>

---

---

```haskell
-- Haskell

f = do
  hd <- openFile "text1" ReadMode

  line <- hGetLine hd
  putStrLn line

  hClose hd
```

---

```c
// C

void f(){
  FILE * fp = fopen("text1", "r");

  char buffer[1024];
  fgets(buffer, 1024, fp);
  buffer[1023] = '\0';
  puts(buffer);

  fclose(fp);
}
```

---

```java
// Java

public class Sample {
  public static void f(){
    FileReader fr = new FileReader("text1");

    char[] buffer = new char[1024];
    fr.read(buffer);
    for(char c : buffer)
      System.out.println(c);

    fr.close();
  }
}
```

---
layout: false

# 稀缺資源使用要避免的問題

  + 用完了沒還

  + 還了之後繼續用

---

# 稀缺資源使用要避免的問題

  + 用完了沒還

      - 就只是忘記還

      - 發生 exception, 醒來以後已經不記得在哪了

      - ...

  + 還了之後繼續用

      - 安排錯歸還時機

      - 忘記曾經把資源的 pointer 或 handler 交給別段程式碼使用

      - ...

---
layout: true

# 使用 bracket 概念的設計

---

```haskell
-- Haskell

f = do
  len <- withFile "text1" ReadMode $ \hd -> do
    line <- hGetLine hd
    putStrLn line
    return (length line)
  putStrLn $ show len
```

---

```cpp
// C++

void f(){
  size_t len;
  {
    ifstream myfile("text1");
    char buffer[1024];
    myfile.getline(buffer, 1023);
    buffer[1023] = '\0';
    string mystring(buffer);
    cout << mystring << endl;
    len = mystring.length();
  }
  cout << len << endl;
};
```

---

```c#
// C#

public class Sample {
  public static void f(){
    int len;
    using( StreamReader sr = new StreamReader("text1") ){
      String line = sr.ReadLine();
      Console.WriteLine(line);
      len = line.Length;
    }
    Console.WriteLine(len);
  }
}

```

--

不是所有的應用情境都可以用, 但當可以用的時候, 這樣使用就不會忘記歸還, 發生 exception 也會自動歸還.

(garbage collection 機制, 由於歸還時間有可能拖很晚, 只適用於供應充足的資源)

---
layout: true

# bracket 不防止歸還之後繼續用

---

```haskell
-- Haskell

f = do
  (len, hd) <- withFile "text1" ReadMode $ \hd -> do
    line <- hGetLine hd
    putStrLn line
    return (length line, hd)
  putStrLn $ show len

* line2 <- hGetLine hd
  putStrLn line2
```

---

```c++
// C++

void f(){
  ifstream * myfile_p;
  size_t len;
  {
    ifstream myfile("text1");
    char buffer[1024];
    myfile.getline(buffer, 1023);
    buffer[1023] = '\0';
    string mystring(buffer);
    cout << mystring << endl;
    len = mystring.length();
    myfile_p = &myfile;
  }
  cout << len << endl;

  char buffer2[1024];
* myfile_p->getline(buffer2, 1023);
  buffer2[1023] = '\0';
  cout << buffer2 << endl;
};
```

---

```c#
// C#

public class Sample {
  public static void f(){
    StreamReader sr_ref;
    int len;
    using( StreamReader sr = new StreamReader("text1") ){
      String line = sr.ReadLine();
      Console.WriteLine(line);
      len = line.Length;
      sr_ref = sr;
    }
    Console.WriteLine(len);

*   String line2 = sr_ref.ReadLine();
    Console.WriteLine(line2);
  }
}

```

---
layout: false

# Lightweight Monadic Region 的目標

--

  + 歸還之後繼續用的程式會 compile error

--

  + Region 可以巢狀使用, 內層的資源會在內層結束時先歸還

--

  + 內層 region 可以使用外層 region 的資源 (內層存續期間外層都存在), 但外層不能使用內層的資源

--

  + 可以從內層 allocate 外層的資源, 這資源是在內層開始以後再取得, 會留到外層結束時才歸還. 也就是我們不一定要 follow 先 allocate 的資源必須較晚歸還的限制.

---

# 一步步打造我們的 library 吧~

<img src=region-io-pdf.png width=100%>

---

# Reference

[Lightweight Monadic Regions - Oleg Kiselyov, Chung-Chieh Shan](http://okmij.org/ftp/Computation/resource-aware-prog/region-io.pdf)
