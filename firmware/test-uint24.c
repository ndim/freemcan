/* Compile this file in order to check whether the compiler supports
 * the __uint24 type as GCC 4.7 and newer do on 8bit AVR.
 */

__uint24 mul3(const __uint24 value);

__uint24 mul3(const __uint24 value)
{
  return (3*value);
}
