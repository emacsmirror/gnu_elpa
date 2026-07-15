// -*- slang-ts-indent-offset: 0 -*-

// Interface with a method.
interface IFoo {
  float2 compute(float3 arg);
// ^ font-lock-type-face
//        ^ font-lock-function-name-face
}

// Structure
struct VertexOutput
// ^ font-lock-keyword-face
//      ^ font-lock-type-face
{
  float3 color;
  float4 v_position : SV_Position;
//                    ^ font-lock-builtin-face
};

[shader("vertex")]
// ^ font-lock-function-call-face
//       ^ font-lock-string-face

void main() {
  let a = 10;
// ^ font-lock-keyword-face
//    ^ font-lock-variable-name-face

  let b = 0.5;
//        ^ font-lock-number-face

  let c = 5 + 3;
//          ^ font-lock-operator-face

   c += 10;
//   ^ font-lock-operator-face

   const int test = 10;
//  ^ font-lock-keyword-face
//        ^ font-lock-type-face
//            ^ font-lock-constant-face

   var grrr = colours[3];
//             ^ font-lock-variable-use-face
}
