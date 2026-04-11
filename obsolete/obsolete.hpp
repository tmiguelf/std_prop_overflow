

	template<class T> requires __OVERFLOW__PRIVATE__::_ovf_suypported<T>::value
	FORCE_INLINE constexpr bool is_div_defined([[maybe_unused]] T const dividend, T const divisor) noexcept
	{
		if constexpr(std::is_signed_v<T>)
		{
			return divisor != 0 && (dividend != std::numeric_limits<T>::min() || divisor != -1);
		}
		else
		{
			return divisor != 0;
		}
	}

	template<class T> requires __OVERFLOW__PRIVATE__::_ovf_suypported<T>::value
	constexpr bool is_div_wide_defined(T dividend_high, [[maybe_unused]] T dividend_low, T divisor) noexcept
	{
		if constexpr(std::is_signed_v<T>)
		{
			using uint_t = std::make_unsigned_t<T>;
			constexpr uintptr_t sign_offset = (sizeof(uint_t) * 8) - 1;
			constexpr uint_t lower_mask = static_cast<uint_t>(~(uint_t{1} << sign_offset));

			uint_t const hi  = std::bit_cast<uint_t>(dividend_high);
			uint_t const low = std::bit_cast<uint_t>(dividend_low);

			uint_t const div = std::bit_cast<uint_t>(divisor);
			uint_t const hi_flag = static_cast<uint_t>((hi << 1) | (low >> sign_offset));

			if(dividend_high < 0)
			{
				if(divisor < 0)
				{
					return
						(hi_flag > div) ||
						((hi_flag == div) &&
							(low & lower_mask));
				}
				else
				{
					uint_t const mirror = ~div;

					return
						(hi_flag > mirror) ||
						((hi_flag == mirror) &&
							((low & lower_mask) > ((mirror & lower_mask) + 1)));
				}
			}
			else
			{
				if(divisor < 0)
				{
					uint_t const mirror = (~div) + 1;

					return
						(hi_flag < mirror) ||
						((hi_flag == mirror) &&
							((low & lower_mask) < mirror));
				}
				else
				{
					return hi_flag < div;
				}
			}
		}
		else
		{
			return dividend_high < divisor;
		}
	}



	template<class T> requires ( __OVERFLOW__PRIVATE__::_ovf_suypported<T>::value && std::is_unsigned_v<T> )
	FORCE_INLINE constexpr div_result<T> div_wide(T dividend_high, T dividend_low, T divisor) noexcept
	{
		if(std::is_constant_evaluated())
		{
			if(!is_div_wide_defined(dividend_high, dividend_low, divisor))
			{
				__OVERFLOW__PRIVATE__::invalid_domain_error();
			}
		}

		using int_t = __OVERFLOW__PRIVATE__::int_clobber_t<T>;
		if constexpr ( std::is_unsigned_v<T> )
		{
#if defined(__PORTABLE_ARCH_AMD64__)
#	if ( defined (__GNUG__) || defined(__GNUC__) )
			if(!std::is_constant_evaluated() && (sizeof(T) != sizeof(uint8_t)))
			{
				__asm__
				(
					"div %[d];"
					: "+a"(dividend_low), "+d"(dividend_high)
					: [d]"r"(divisor)
				);

				return {.quotient = dividend_low, .remainder = dividend_high};
			}
			else
#	elif defined(_WIN32)
			if(!std::is_constant_evaluated() && sizeof(T) == sizeof(uint64_t))
			{
				uint64_t rem;
				uint64_t const quo =
					_udiv128(
						static_cast<uint64_t>(dividend_high), static_cast<uint64_t>(dividend_low),
						static_cast<uint64_t>(divisor), &rem
					);
				return {.quotient = static_cast<T>(quo), .remainder = static_cast<T>(rem)};
			}
			else
#	endif
#endif
			{
				if constexpr (sizeof(T) == sizeof(uint64_t))
				{
					if(dividend_high >= divisor)
					{
						return {.quotient = 0, .remainder = 0};
					}

					T result = 0;

					if(dividend_high)
					{
						auto d_pad = std::countl_zero(divisor);

						if(std::popcount(divisor) == 1)
						{
							++d_pad;
							constexpr T rem_mask = 0xFFFFFFFFFFFFFFFF;

							return {.quotient = (dividend_high << d_pad) | (dividend_low >> (64 - d_pad)),
								.remainder = dividend_low & (rem_mask >> d_pad)};
						}

						if(d_pad < 16)
						{
							T const estimator = static_cast<T>(divisor >> 32) + 1;

							do
							{
								auto const dhi_pad = std::countl_zero(dividend_high);
								T estimate = (dhi_pad ? ((dividend_high << dhi_pad) | (dividend_low >> (64 - dhi_pad))) : dividend_high) / estimator;
								if(dhi_pad < 32)
								{
									estimate <<= (32 - dhi_pad);
								}
								else
								{
									estimate >>= (dhi_pad - 32);
								}

								result += estimate;

								mul_wide_result const res = mul_wide(divisor, estimate);

								{
									sub_borrow_result sub_res = sub_borrow(dividend_low, res.low_bits, false);
									dividend_low = sub_res.low_bits;
									sub_res = sub_borrow(dividend_high, res.high_bits, sub_res.overflow);
									dividend_high = sub_res.low_bits;
								}
							}
							while(dividend_high);
						}
						else
						{
							do
							{
								auto const dhi_pad = std::countl_zero(dividend_high);
								T estimate = ((dividend_high << dhi_pad) | (dividend_low >> (64 - dhi_pad))) / divisor;
								estimate <<= (64 - dhi_pad);
								result += estimate;

								mul_wide_result const res = mul_wide(divisor, estimate);

								{
									sub_borrow_result sub_res = sub_borrow(dividend_low, res.low_bits, false);
									dividend_low = sub_res.low_bits;
									sub_res = sub_borrow(dividend_high, res.high_bits, sub_res.overflow);
									dividend_high = sub_res.low_bits;
								}
							}
							while(dividend_high);
						}
					}

					{
						result += dividend_low / divisor;
						dividend_low %= divisor;
					}

					return {.quotient = result, .remainder = dividend_low};
				}
				else
				{
					using over_uint_t = __OVERFLOW__PRIVATE__::int_clobber_type<sizeof(int_t) * 2, false>::type;
					constexpr uint8_t offset = (sizeof(int_t) * 8);
					over_uint_t const dividend = static_cast<over_uint_t>((static_cast<over_uint_t>(dividend_high) << offset) | static_cast<over_uint_t>(dividend_low));
					return {.quotient = static_cast<T>(dividend / divisor), .remainder = static_cast<T>(dividend % divisor)};
				}
			}
		}
#if 0
		else if constexpr ( std::is_signed_v<T> )
		{
#if defined(__PORTABLE_ARCH_AMD64__)
#	if ( defined (__GNUG__) || defined(__GNUC__) )
			if(!std::is_constant_evaluated() && (sizeof(T) != sizeof(int8_t)))
			{
				__asm__
				(
					"idiv %[d];"
					: "+a"(dividend_low), "+d"(dividend_high)
					: [d]"r"(divisor)
				);

				return {.quotient = dividend_low, .remainder = dividend_high};
			}
			else
#	elif defined(_WIN32)
			if(!std::is_constant_evaluated() && sizeof(T) == sizeof(int64_t))
			{
				int64_t rem;
				int64_t const quo =
					_div128(
						static_cast<int64_t>(dividend_high), static_cast<int64_t>(dividend_low),
						static_cast<int64_t>(divisor), &rem
					);
				return {.quotient = static_cast<T>(quo), .remainder = static_cast<T>(rem)};
			}
			else
#	endif
#endif
			{
				if constexpr (sizeof(T) == sizeof(int64_t))
				{
					bool swap = false;
					bool rem_negative = false;
					uint64_t tdivisor = static_cast<uint64_t>(divisor);
					uint64_t tdiv_hi = static_cast<uint64_t>(dividend_high);
					uint64_t tdiv_lo = static_cast<uint64_t>(dividend_low);

					if(divisor < 0)
					{
						swap = true;
						tdivisor = ~tdivisor + 1;
					}
					if(dividend_high < 0)
					{
						swap = !swap;
						rem_negative = true;
						tdiv_lo = ~tdiv_lo + 1;
						tdiv_hi = ~tdiv_hi;
						if(tdiv_lo == 0)
						{
							tdiv_hi +=1;
						}
					}

					std::div_result<uint64_t> const tempRes = std::div_wide<uint64_t>(tdiv_hi, tdiv_lo, tdivisor);

					if(tempRes.quotient == 0x8000000000000000)
					{
						return
						{
							.quotient  = static_cast<T>(tempRes.quotient),
							.remainder = rem_negative ? -static_cast<T>(tempRes.remainder) : static_cast<T>(tempRes.remainder)
						};
					}

					return
					{
						.quotient  = swap         ? -static_cast<T>(tempRes.quotient)  : static_cast<T>(tempRes.quotient),
						.remainder = rem_negative ? -static_cast<T>(tempRes.remainder) : static_cast<T>(tempRes.remainder)
					};
				}
				else
				{
					using clober_uint_t = __OVERFLOW__PRIVATE__::int_clobber_type<sizeof(int_t), false>::type;
					using over_sint_t = __OVERFLOW__PRIVATE__::int_clobber_type<sizeof(int_t) * 2, true>::type;
					constexpr uint8_t offset = (sizeof(int_t) * 8);

					over_sint_t const dividend = static_cast<over_sint_t>((static_cast<over_sint_t>(dividend_high) << offset) 
						| static_cast<over_sint_t>(static_cast<clober_uint_t>(dividend_low)));

					return {.quotient = static_cast<T>(dividend / divisor), .remainder = static_cast<T>(dividend % divisor)};
				}
			}
		}
#endif
	}


	template<class T> requires ( __OVERFLOW__PRIVATE__::_ovf_suypported<T>::value  )
	FORCE_INLINE constexpr mul_wide_result<T> mul_add_wide(T vA, T const vB, T const addend) noexcept
	{
		using int_t = __OVERFLOW__PRIVATE__::int_clobber_t<T>;
		if constexpr (std::is_unsigned_v<T>)
		{
#if defined(__PORTABLE_ARCH_AMD64__)
#	if ( defined (__GNUG__) || defined(__GNUC__) )
			if (!std::is_constant_evaluated() && sizeof(T) != sizeof(uint8_t))
			{
				int_t hi_res;
				__asm__
				(
					"mul %[b];"
					: "+a"(vA), "=d"(hi_res)
					: [b] "r"(vB)
				);

				__asm__
				(
					"add %[b], %[a];"
					"adc %[z], %[c];"
					: [a] "+rm"(vA), [c]"+rm"(hi_res)
					: [b] "rm"(addend), [z]"n"(0)
				);

				return { .low_bits = static_cast<T>(vA), .high_bits = static_cast<T>(hi_res) };
			}
			else
#	elif defined(_WIN32)
			if (!std::is_constant_evaluated() && sizeof(T) == sizeof(uint64_t))
			{
				uint64_t res_high;
				uint64_t res_low = _umul128(static_cast<uint64_t>(vA), static_cast<uint64_t>(vB), &res_high);
				res_high += _addcarry_u64(0, addend, &res_low);

				return { .low_bits = static_cast<T>(res_low), .high_bits = static_cast<T>(res_high) };
			}
			else if (!std::is_constant_evaluated() && sizeof(T) == sizeof(uint32_t))
			{
				uint64_t const res = __emulu(static_cast<unsigned int>(vA), static_cast<unsigned int>(vB)) + static_cast<uint64_t>(addend);
				return { .low_bits = static_cast<T>(res), .high_bits = static_cast<T>(res >> 32) };
			}
			else
#	endif
#endif
			{
				if constexpr (sizeof(T) == sizeof(uint64_t))
				{

					constexpr uint64_t low_mask = 0xFFFFFFFF;
					uint64_t const Al = static_cast<uint64_t>(vA) & low_mask;
					uint64_t const Ah = static_cast<uint64_t>(vA) >> 32;
					uint64_t const Bl = static_cast<uint64_t>(vB) & low_mask;
					uint64_t const Bh = static_cast<uint64_t>(vB) >> 32;

					add_carry_result<uint64_t> const addc_result_l = add_carry<uint64_t>(vA * vB, addend, 0);
					add_carry_result<uint64_t> const addc_result_h = add_carry<uint64_t>(Al * Bh + ((Al * Bl) >> 32), Ah * Bl, 0);

					return
					{
						.low_bits = addc_result_l.low_bits,
						.high_bits = static_cast<T>((Ah * Bh) + (addc_result_h.low_bits >> 32) + (addc_result_h.overflow ? 0x100000000 : 0)) + addc_result_l.overflow
					};
				}
				else
				{
					using over_uint_t = __OVERFLOW__PRIVATE__::int_clobber_type<sizeof(int_t) * 2, false>::type;
					constexpr uint8_t offset = (sizeof(int_t) * 8);
					over_uint_t const temp_result = static_cast<over_uint_t>(static_cast<over_uint_t>(vA) * static_cast<over_uint_t>(vB) + static_cast<over_uint_t>(addend));
					return { .low_bits = static_cast<T>(temp_result), .high_bits = static_cast<T>(temp_result >> offset) };
				}
			}
		}
		else if constexpr (std::is_signed_v<T>)
		{

		}
		return { .low_bits = static_cast<T>(0), .high_bits = static_cast<T>(0) };
	}

	template<class T> requires __OVERFLOW__PRIVATE__::_ovf_suypported<T>::value
	FORCE_INLINE constexpr mul_wide_result<T> mul_add2_wide(T vA, T const vB, T const addendA, T const addendB) noexcept
	{

	}


TYPED_TEST(numeric_ovf_typed_test_T, mull_add_wide)
{
	using num_T = TypeParam;

	struct TestCase_t
	{
		num_T const vA;
		num_T const vB;
		num_T const addend;
		num_T const e_low;
		num_T const e_hi;

		std::string format() const
		{
			return std::format("vA={} vB={} added={}", vA, vB, addend);
		}

		consteval bool constexpr_check() const
		{
			{
				std::mul_wide_result const result = std::mul_add_wide(vA, vB, addend);
				if (result.low_bits != e_low || result.high_bits != e_hi)
				{
					return false;
				}
			}
			if (vA != vB)
			{
				std::mul_wide_result const result = std::mul_add_wide(vB, vA, addend);
				if (result.low_bits != e_low || result.high_bits != e_hi)
				{
					return false;
				}
			}
			return true;
		}
	};


	constexpr num_T max = std::numeric_limits<num_T>::max();

	if constexpr (std::is_unsigned_v<num_T>)
	{
		std::array constexpr testCases
		{
			TestCase_t{.vA = 0, .vB = 0, .addend = 0, .e_low = 0, .e_hi = 0},
			TestCase_t{.vA = max, .vB = max, .addend = max, .e_low = 0, .e_hi = max},
		};

		constexpr bool constexpr_result = constexpr_test(testCases);
		constexpr uintptr_t constexpr_findex = constexpr_fail_index(testCases);

		ASSERT_TRUE(constexpr_result) << "constexpr " << testCases[constexpr_findex].format();

		for (TestCase_t const& testCase : testCases)
		{
			{
				std::mul_wide_result const result = std::mul_add_wide(testCase.vA, testCase.vB, testCase.addend);
				ASSERT_EQ(result.low_bits, testCase.e_low) << testCase.format();
				ASSERT_EQ(result.high_bits, testCase.e_hi) << testCase.format();
			}
			if (testCase.vA != testCase.vB)
			{
				std::mul_wide_result const result = std::mul_add_wide(testCase.vB, testCase.vA, testCase.addend);
				ASSERT_EQ(result.low_bits, testCase.e_low) << testCase.format();
				ASSERT_EQ(result.high_bits, testCase.e_hi) << testCase.format();
			}
		}
	}
}
