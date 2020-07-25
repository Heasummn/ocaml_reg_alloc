open Register;;

type interval =
  { name: string;
    reg: regs;
    location: int;
    start_point: int;
    end_point: int;
  }

let compare_endpoint int1 int2 =
  Int.compare int1.end_point int2.end_point

let compare_startpoint int1 int2 =
  Int.compare int1.start_point int2.start_point

let make_interval name start close =
  {
    name = name;
    reg = NoReg;
    location = -1;
    start_point = start;
    end_point = close;
  }