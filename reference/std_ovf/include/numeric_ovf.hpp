
#pragma once

#include <numeric>
#include <limits>
#include <type_traits>
#include <cstdint>
#include <bit>

#if defined(_M_AMD64) || defined(__amd64__)
#	define __PORTABLE_ARCH_AMD64__
#	ifdef _WIN32
#		include <intrin.h>
#	endif
#	include <immintrin.h>
#else

#endif

#if ( defined (__GNUG__) || defined(__GNUC__) )
#	define FORCE_INLINE __attribute__((always_inline))
#elif ( defined (_MSC_VER ) )
#	define FORCE_INLINE __forceinline
#else
#	define FORCE_INLINE
#endif

//__readeflags

namespace std
{
	namespace __OVERFLOW__PRIVATE__
	{
		template<class T>
		struct _ovf_is_base_type : false_type {};

		template<class T> requires ( 
			std::same_as<T, uint8_t > ||
			std::same_as<T, uint16_t> ||
			std::same_as<T, uint32_t> ||
			std::same_as<T, uint64_t> ||
			std::same_as<T, int8_t >  ||
			std::same_as<T, int16_t>  ||
			std::same_as<T, int32_t>  ||
			std::same_as<T, int64_t>
			)
		struct _ovf_is_base_type<T>  : true_type {};

		template<class T>
		struct _ovf_suypported : false_type {};

		template<class T> requires ( 
			_ovf_is_base_type<T>::value         ||
			std::same_as<T, short>              ||
			std::same_as<T, unsigned short>     ||
			std::same_as<T, int>                ||
			std::same_as<T, unsigned int>       ||
			std::same_as<T, long>               ||
			std::same_as<T, unsigned long>      ||
			std::same_as<T, long long>          ||
			std::same_as<T, unsigned long long>
			)
		struct _ovf_suypported<T> : true_type {};


		template<uintptr_t TSIZE, bool TSIGNED>
		struct int_clobber_type;

		template<> struct int_clobber_type<sizeof(uint8_t ), false> { using type = uint8_t ; };
		template<> struct int_clobber_type<sizeof(uint16_t), false> { using type = uint16_t; };
		template<> struct int_clobber_type<sizeof(uint32_t), false> { using type = uint32_t; };
		template<> struct int_clobber_type<sizeof(uint64_t), false> { using type = uint64_t; };
		template<> struct int_clobber_type<sizeof(int8_t ), true> { using type = int8_t ; };
		template<> struct int_clobber_type<sizeof(int16_t), true> { using type = int16_t; };
		template<> struct int_clobber_type<sizeof(int32_t), true> { using type = int32_t; };
		template<> struct int_clobber_type<sizeof(int64_t), true> { using type = int64_t; };

		template<typename T>
		using int_clobber_t = int_clobber_type<sizeof(T), std::is_signed_v<T>>::type;

		static inline void invalid_domain_error(){}
	} //namespace __OVERFLOW__PRIVATE__

	template<class T> requires __OVERFLOW__PRIVATE__::_ovf_suypported<T>::value
	struct add_carry_result
	{
		T low_bits;
		bool overflow;
	};

	template<class T>
	using sub_borrow_result = add_carry_result<T>;

	template<class T> requires __OVERFLOW__PRIVATE__::_ovf_suypported<T>::value
	struct mul_wide_result {
		T low_bits;
		T high_bits;
	};

	template<class T> requires __OVERFLOW__PRIVATE__::_ovf_suypported<T>::value
	struct div_result {
		T quotient;
		T remainder;
	};


	template<class T> requires __OVERFLOW__PRIVATE__::_ovf_suypported<T>::value
	FORCE_INLINE constexpr add_carry_result<T> add_carry(T vA, T const vB, bool carry) noexcept
	{
		using int_t = __OVERFLOW__PRIVATE__::int_clobber_t<T>;
		if constexpr (std::is_unsigned_v<T>)
		{
#ifdef __PORTABLE_ARCH_AMD64__
#	if ( defined (__GNUG__) || defined(__GNUC__) )
			if(!std::is_constant_evaluated())
			{
				uint16_t const aux = carry;
				__asm__
				(
					"bt %[z], %[r];"
					"adc %[b], %[a];"
					"setc %[c];"
					: [a]"+rm"(vA), [c]"=rm"(carry)
					: [b]"rm"(vB), [r]"r"(aux), [z]"n"(0)
				);

				return {.low_bits = vA, .overflow = carry};
			}
			else
#	elif defined(_WIN32)
			if(!std::is_constant_evaluated())
			{
				if constexpr (sizeof(T) == sizeof(uint64_t))
				{
					unsigned long long res;
					bool const rcarry = _addcarry_u64(static_cast<uint8_t>(carry), vA, vB, &res);
					return {.low_bits = static_cast<T>(res), .overflow = rcarry};
				}
				else if constexpr (sizeof(T) == sizeof(uint32_t))
				{
					uint32_t res;
					bool const rcarry = _addcarry_u32(static_cast<uint8_t>(carry), vA, vB, &res);
					return {.low_bits = static_cast<T>(res), .overflow = rcarry};
				}
				else if constexpr (sizeof(T) == sizeof(uint16_t))
				{
					uint16_t res;
					bool const rcarry = _addcarry_u16(static_cast<uint8_t>(carry), vA, vB, &res);
					return {.low_bits = static_cast<T>(res), .overflow = rcarry};
				}
				else if constexpr (sizeof(T) == sizeof(uint8_t))
				{
					uint8_t res;
					bool const rcarry = _addcarry_u8(static_cast<uint8_t>(carry), vA, vB, &res);
					return {.low_bits = static_cast<T>(res), .overflow = rcarry};
				}
			}
			else
#	endif
#endif // __PORTABLE_ARCH_AMD64__
			{
				if constexpr (sizeof(T) < sizeof(uint64_t))
				{
					using over_uint_t = __OVERFLOW__PRIVATE__::int_clobber_type<sizeof(int_t) * 2, false>::type;
					constexpr over_uint_t test_mask = over_uint_t{1} << (sizeof(int_t) * 8);
					over_uint_t const temp_result = static_cast<over_uint_t>(static_cast<over_uint_t>(vA) + static_cast<over_uint_t>(vB) + static_cast<over_uint_t>(carry));
					return {.low_bits = static_cast<int_t>(temp_result), .overflow = (temp_result & test_mask) ? true : false};
				}
				else
				{
					uint64_t res_low = static_cast<uint64_t>(vA) + static_cast<uint64_t>(vB);
					bool const ovf = (res_low < static_cast<uint64_t>(vA));
					if(carry)
					{
						++res_low;
					}
					return {.low_bits = static_cast<T>(res_low), .overflow = ovf || ((res_low == 0) && carry)};
				}
			}

		}
		else if constexpr ( std::is_signed_v<T> )
		{
#if defined(__PORTABLE_ARCH_AMD64__)
#	if ( defined (__GNUG__) || defined(__GNUC__) )
			if(!std::is_constant_evaluated())
			{
				uint16_t const aux = carry;
				__asm__
				(
					"bt %[z], %[r];"
					"adc %[b], %[a];"
					"seto %[o];"
					: [a]"+rm"(vA), [o]"=rm"(carry)
					: [b]"rm"(vB), [r]"r"(aux), [z]"n"(0)
				);

				return {.low_bits = vA, .overflow = carry};
			}
			else
#	endif // gcc
#endif // __PORTABLE_ARCH_AMD64__
			{
				using uint_clober_t = __OVERFLOW__PRIVATE__::int_clobber_type<sizeof(T), false>::type;

				constexpr T mask = static_cast<T>(T{1} << (sizeof(T) * 8 - 1));
				T res_low = static_cast<T>(std::bit_cast<uint_clober_t>(vA) + std::bit_cast<uint_clober_t>(vB));
				if(carry)
				{
					res_low = static_cast<T>(std::bit_cast<uint_clober_t>(res_low) + uint_clober_t{1});
				}

				T const vA_sign = (mask & vA);
				
				return {.low_bits = res_low, .overflow = ((vA_sign == (mask & vB)) && (vA_sign != (mask & res_low)))};
			}
		}
	}

	template<class T> requires __OVERFLOW__PRIVATE__::_ovf_suypported<T>::value
	FORCE_INLINE constexpr sub_borrow_result<T> sub_borrow(T vA, T const vB, bool borrow) noexcept
	{
		using int_t = __OVERFLOW__PRIVATE__::int_clobber_t<T>;
		if constexpr (std::is_unsigned_v<T>)
		{
#if defined(__PORTABLE_ARCH_AMD64__)
#	if ( defined (__GNUG__) || defined(__GNUC__) )
			if(!std::is_constant_evaluated())
			{
				uint16_t const aux = borrow;
				__asm__
				(
					"bt %[z], %[r];"
					"sbb %[b], %[a];"
					"setc %[c];"
					: [a]"+rm"(vA), [c]"=rm"(borrow)
					: [b]"rm"(vB), [r]"r"(aux), [z]"n"(0)
				);

				return {.low_bits = vA, .overflow = borrow};
			}
			else
#	elif defined(_WIN32)
			if(!std::is_constant_evaluated())
			{
				if constexpr (sizeof(T) == sizeof(uint64_t))
				{
					unsigned long long res;
					bool const rcarry = _subborrow_u64(static_cast<uint8_t>(borrow), vA, vB, &res);
					return {.low_bits = static_cast<T>(res), .overflow = rcarry};
				}
				else if constexpr (sizeof(T) == sizeof(uint32_t))
				{
					uint32_t res;
					bool const rcarry = _subborrow_u32(static_cast<uint8_t>(borrow), vA, vB, &res);
					return {.low_bits = static_cast<T>(res), .overflow = rcarry};
				}
				else if constexpr (sizeof(T) == sizeof(uint16_t))
				{
					uint16_t res;
					bool const rcarry = _subborrow_u16(static_cast<uint8_t>(borrow), vA, vB, &res);
					return {.low_bits = static_cast<T>(res), .overflow = rcarry};
				}
				else if constexpr (sizeof(T) == sizeof(uint8_t))
				{
					uint8_t res;
					bool const rcarry = _subborrow_u8(static_cast<uint8_t>(borrow), vA, vB, &res);
					return {.low_bits = static_cast<T>(res), .overflow = rcarry};
				}
			}
			else
#	endif
#endif
			{
				if constexpr (sizeof(T) < sizeof(uint64_t))
				{
					using over_uint_t = __OVERFLOW__PRIVATE__::int_clobber_type<sizeof(int_t) * 2, false>::type;
					constexpr over_uint_t test_mask = over_uint_t{1} << (sizeof(int_t) * 8);
					over_uint_t const temp_result = static_cast<over_uint_t>(static_cast<over_uint_t>(vA) - static_cast<over_uint_t>(vB) - static_cast<over_uint_t>(borrow));
					return { .low_bits = static_cast<int_t>(temp_result), .overflow = (temp_result & test_mask) ? true : false };
				}
				else
				{
					uint64_t res_low = static_cast<uint64_t>(vA) - static_cast<uint64_t>(vB);
					bool const ovf = (res_low > static_cast<uint64_t>(vA));
					if(borrow)
					{
						--res_low;
					}
					return {.low_bits = static_cast<T>(res_low), .overflow = ovf || ((res_low == std::numeric_limits<uint64_t>::max()) && borrow)};
				}
			}
		}
		else if constexpr ( std::is_signed_v<T> )
		{
#if defined(__PORTABLE_ARCH_AMD64__)
#	if ( defined (__GNUG__) || defined(__GNUC__) )
			if(!std::is_constant_evaluated())
			{
				uint16_t const aux = borrow;
				__asm__
				(
					"bt %[z], %[r];"
					"sbb %[b], %[a];"
					"seto %[o];"
					: [a]"+rm"(vA), [o]"=rm"(borrow)
					: [b]"rm"(vB), [r]"r"(aux), [z]"n"(0)
				);

				return { .low_bits = vA, .overflow = borrow };
			}
			else
#	endif // gcc
#endif
			{
				using uint_clober_t = __OVERFLOW__PRIVATE__::int_clobber_type<sizeof(T), false>::type;

				constexpr T mask = static_cast<T>(T{1} << (sizeof(T) * 8 - 1));

				T res_low = static_cast<T>(std::bit_cast<uint_clober_t>(vA) - std::bit_cast<uint_clober_t>(vB));
				if(borrow)
				{
					res_low = static_cast<T>(std::bit_cast<uint_clober_t>(res_low) - uint_clober_t{1});
				}

				T const vA_sign = (mask & vA);

				return {.low_bits = res_low, .overflow = ((vA_sign != (mask & vB)) && (vA_sign != (mask & res_low)))};
			}
		}
	}

	template<class T> requires __OVERFLOW__PRIVATE__::_ovf_suypported<T>::value
	FORCE_INLINE constexpr mul_wide_result<T> mul_wide(T vA, T const vB) noexcept
	{
		using int_t = __OVERFLOW__PRIVATE__::int_clobber_t<T>;
		if constexpr ( std::is_unsigned_v<T> )
		{
#if defined(__PORTABLE_ARCH_AMD64__)
#	if ( defined (__GNUG__) || defined(__GNUC__) )
			if(!std::is_constant_evaluated() && sizeof(T) != sizeof(uint8_t))
			{
				int_t hi_res;
				__asm__
				(
					"mul %[b];"
					: "+a"(vA), "=d"(hi_res)
					: [b]"r"(vB)
				);
				return {.low_bits = static_cast<T>(vA), .high_bits = static_cast<T>(hi_res)};
			}
			else
#	elif defined(_WIN32)
			if(!std::is_constant_evaluated() && sizeof(T) == sizeof(uint64_t))
			{
				uint64_t res_high;
				uint64_t const res_low = _umul128(static_cast<uint64_t>(vA), static_cast<uint64_t>(vB), &res_high);
				return { .low_bits = static_cast<T>(res_low), .high_bits = static_cast<T>(res_high)};
			}
			else if(!std::is_constant_evaluated() && sizeof(T) == sizeof(uint32_t))
			{
				uint64_t const res = __emulu(static_cast<unsigned int>(vA), static_cast<unsigned int>(vB));
				return { .low_bits = static_cast<T>(res), .high_bits = static_cast<T>(res >> 32)};
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

					add_carry_result<uint64_t> const addc_result = add_carry<uint64_t>( Al * Bh + ((Al * Bl) >> 32), Ah * Bl, 0);

					return
					{
						.low_bits = (vA * vB),
						.high_bits = static_cast<T>((Ah * Bh) + (addc_result.low_bits >> 32) + (addc_result.overflow ? 0x100000000 : 0))
					};

				}
				else
				{
					using over_uint_t = __OVERFLOW__PRIVATE__::int_clobber_type<sizeof(int_t) * 2, false>::type;
					constexpr uint8_t offset = (sizeof(int_t) * 8);
					over_uint_t const temp_result = static_cast<over_uint_t>(static_cast<over_uint_t>(vA) * static_cast<over_uint_t>(vB));
					return { .low_bits = static_cast<T>(temp_result), .high_bits = static_cast<T>(temp_result >> offset)};
				}
			}
		}
		else if constexpr ( std::is_signed_v<T> )
		{
#if defined(__PORTABLE_ARCH_AMD64__)
#	if ( defined (__GNUG__) || defined(__GNUC__) )
			if(!std::is_constant_evaluated() && sizeof(T) != sizeof(int8_t))
			{
				int_t hi_res;
				__asm__
				(
					"imul %[b];"
					: "+a"(vA), "=d"(hi_res)
					: [b]"r"(vB)
				);
				return {.low_bits = static_cast<T>(vA), .high_bits = static_cast<T>(hi_res)};
			}
		else
#	elif defined(_WIN32)
			if(!std::is_constant_evaluated() && sizeof(T) == sizeof(int64_t))
			{
				int64_t res_high;
				int64_t const res_low = _mul128(static_cast<int64_t>(vA), static_cast<int64_t>(vB), &res_high);
				return { .low_bits = static_cast<T>(res_low), .high_bits = static_cast<T>(res_high)};
			}
			else if(!std::is_constant_evaluated() && sizeof(T) == sizeof(int32_t))
			{
				int64_t const res = __emul(static_cast<unsigned int>(vA), static_cast<unsigned int>(vB));
				return { .low_bits = static_cast<T>(res), .high_bits = static_cast<T>(res >> 32)};
			}
			else
#	endif
#endif
			{
				if constexpr (sizeof(T) == sizeof(int64_t))
				{
					constexpr uint64_t low_mask = 0xFFFFFFFF;
					constexpr uint64_t sign_mask = 0x8000000000000000;

					uint64_t const tA = static_cast<uint64_t>(vA);
					uint64_t const tB = static_cast<uint64_t>(vB);

					uint64_t const Al = tA & low_mask;
					uint64_t const Ah = tA >> 32;
					uint64_t const Bl = tB & low_mask;
					uint64_t const Bh = tB >> 32;

					add_carry_result<uint64_t> const addc_result = add_carry<uint64_t>( Al * Bh + ((Al * Bl) >> 32), Ah * Bl, 0);
					uint64_t high_bits = static_cast<T>((Ah * Bh) + (addc_result.low_bits >> 32) + (addc_result.overflow ? 0x100000000 : 0));

					if(sign_mask & tA)
					{
						high_bits -= tB;
					}
					if(sign_mask & tB)
					{
						high_bits -= tA;
					}

					return {.low_bits = static_cast<T>(tA * tB), .high_bits = static_cast<T>(high_bits)};
				}
				else
				{
					using over_sint_t = __OVERFLOW__PRIVATE__::int_clobber_type<sizeof(int_t) * 2, true>::type;
					constexpr uint8_t offset = (sizeof(int_t) * 8);
					over_sint_t const temp_result = static_cast<over_sint_t>(static_cast<over_sint_t>(vA) * static_cast<over_sint_t>(vB));
					return { .low_bits = static_cast<T>(temp_result), .high_bits = static_cast<T>(temp_result >> offset)};
				}
			}
		}
	};

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

	template<class T> requires __OVERFLOW__PRIVATE__::_ovf_suypported<T>::value
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

	};

} //namespace std

