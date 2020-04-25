#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <set>
#include <vector>

int main(int argc, char *argv[])
{   
	std::string val;

	std::istringstream iss_upto(argv[1]);
	iss_upto >> val;
	int max = 24;
	int count = 0;

	int a = std::stoi(val);
	int b = 0;
	int c = 0;
	int d = 0;

	d = a;
	d += 2534;

	while (true)
	{
		a = d;
		do
		{ 
			b = a;
			a = 0;
			while (true)
			{ 
				c = 2;
				if (b != 0)
				{
					--b;
					--c;
				}
				else
					break;

				if (b != 0)
				{
					--b;
					--c;
				}
				else
					break;

				++a;
			}
			b = 2 - c;

			std::cout << b;
			++count;
			if (count == max)
			{
				std::cout << std::endl;
				return 0;
			}
		} while (a != 0);
	} 
	return 0;
}